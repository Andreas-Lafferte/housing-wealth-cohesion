#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analysis code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to perform regression analysis
# Date: January 20, 2026
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               psych,
               here,
               rlang,
               sjlabelled,
               visdat,
               sjmisc, 
               estimatr,
               sandwich,
               marginaleffects,
               ggeffects,
               glue,
               texreg,
               broom)

library(conflicted)
conflict_prefer_all("dplyr", "tidyr")
conflict_prefer_all("dplyr", "stats")
conflicted::conflicts_prefer(dplyr::filter)
conflict_prefer_all("dplyr", "tidylog")
conflict_prefer_all("tidyr", "tidylog")

library(ggdist)
ggplot2::theme_set(theme_ggdist())
# 2. Data -----------------------------------------------------------------

load(here("output/data/df_study1_V2.RData"))
load(here("output/data/db_long_V2.RData"))

glimpse(df_study1)
conflicts_prefer(dplyr::filter)
# convert wave to factor for FE
df_study1 <- df_study1 %>% 
  mutate(ola = factor(ola, levels = c("2016", "2017", "2018", "2019")),
         ola = sjlabelled::set_label(ola, "Wave"),
         housing = factor(housing, 
                          levels = c("Owned home with mortgage payments",
                                     "Owned and fully paid-off home",
                                     "Rented housing",
                                     "Other regime")),
         age_2 = (age)^2)

# 3. Function -------------------------------------------------------------

estimate_lm_robust <- function(
    vardep, 
    pred1 = "housing",
    pred2 = "ola", 
    pred3 = "age", 
    cluster = "idencuesta",
    controls = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
    datos = df_study1, 
    transform = FALSE, 
    relevel = FALSE, 
    relevel_cat = 3
) {
  # Transform pred1 to factor if necessary
  if (transform) {
    datos[[pred1]] <- to_label(datos[[pred1]])
  }
  
  # Relevel pred1 if necessary
  if (relevel) {
    datos[[pred1]] <- relevel(as.factor(datos[[pred1]]), ref = relevel_cat)
  }
  
  # Create string of controls separated by "+"
  controls_str <- paste(controls, collapse = " + ")
  
  # Create dynamic formulas and estimate models
  forms <- c(
    glue("{vardep} ~ {pred1} + {pred2}"),
    glue("{vardep} ~ {pred1} + {pred2} + {pred3}"),
    glue("{vardep} ~ {pred1} + {pred2} + {pred3} + {controls[1]}"),
    glue("{vardep} ~ {pred1} + {pred2} + {pred3} + {controls[1]} + {controls[2]}"),
    glue("{vardep} ~ {pred1} + {pred2} + {pred3} + {controls[1]} + {controls[2]} + {controls[3]}")
  )
  
  models <- map(
    forms,
    ~ lm_robust(
      as.formula(.x),
      data = datos,
      se_type = "CR2",
      clusters = datos[[cluster]]
    )
  )
  
  return(models)
}


df_int <- subset(df_study1, 
                 housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")) %>% 
  mutate(
    housing = factor(housing, 
                     levels = c("Owned and fully paid-off home", 
                                "Owned home with mortgage payments"))
  )

estimate_lm_robust_interact2sets <- function(
    vardep,
    housing = "housing",                 # moderador dicotómico (factor 2 niveles)
    pred_price = "ln_uf2018",            # continuo (log land price)
    pred_decile = "dummy_decile_uf2018", # dummy (p.ej., decil 10)
    wave = "ola",                        # wave FE
    age = "age",
    cluster = "idencuesta",
    controls = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
    datos = df_int,
    transform = FALSE,
    ref_housing = "Owned and fully paid-off home"
) {
  
  # Helper: run nested models given a "wealth" predictor (continuous or dummy)
  run_set <- function(pred_wealth) {
    
    base_terms <- glue("{housing} + {pred_wealth} + {housing}:{pred_wealth} + {wave}")
    
    forms <- c(
      glue("{vardep} ~ {base_terms}"),
      glue("{vardep} ~ {base_terms} + {age}"),
      glue("{vardep} ~ {base_terms} + {age} + {controls[1]}"),
      glue("{vardep} ~ {base_terms} + {age} + {controls[1]} + {controls[2]}"),
      glue("{vardep} ~ {base_terms} + {age} + {controls[1]} + {controls[2]} + {controls[3]}")
    )
    
    models <- map(
      forms,
      ~ lm_robust(
        formula  = as.formula(.x),
        data     = datos,
        se_type  = "CR2",
        clusters = datos[[cluster]]
      )
    )
    
    list(
      pred_wealth = pred_wealth,
      formulas = forms,
      models = models
    )
  }
  
  out <- list(
    set_price  = run_set(pred_price),
    set_decile = run_set(pred_decile)
  )
  
  return(out)
}

estimate_lm_robust_interact_decile_cluster <- function(
    vardep,
    cluster_c   = "cluster_c",              # moderador (character "1"..."5")
    pred_decile = "dummy_decile_uf2018",    # predictor (dummy)
    wave        = "ola",                    # wave FE
    age         = "age",
    cluster     = "idencuesta",
    controls    = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
    datos       = df_int,
    transform   = FALSE,
    ref_cluster = "1"                       # nivel de referencia del moderador
) {
  
  stopifnot(length(controls) >= 3)
  stopifnot(all(c(vardep, cluster_c, pred_decile, wave, age, cluster) %in% names(datos)))
  
  # asegurar factor 1..5 aunque venga como character
  datos <- dplyr::mutate(
    datos,
    !!cluster_c := factor(.data[[cluster_c]], levels = as.character(1:5))
  )
  
  # fijar referencia si existe
  if (!is.null(ref_cluster) && ref_cluster %in% levels(datos[[cluster_c]])) {
    datos[[cluster_c]] <- stats::relevel(datos[[cluster_c]], ref = ref_cluster)
  }
  
  base_terms <- glue::glue("{cluster_c} + {pred_decile} + {cluster_c}:{pred_decile} + {wave}")
  
  forms <- c(
    glue::glue("{vardep} ~ {base_terms}"),
    glue::glue("{vardep} ~ {base_terms} + {age}"),
    glue::glue("{vardep} ~ {base_terms} + {age} + {controls[1]}"),
    glue::glue("{vardep} ~ {base_terms} + {age} + {controls[1]} + {controls[2]}"),
    glue::glue("{vardep} ~ {base_terms} + {age} + {controls[1]} + {controls[2]} + {controls[3]}")
  )
  
  models <- purrr::map(
    forms,
    ~ estimatr::lm_robust(
      formula  = stats::as.formula(.x),
      data     = datos,
      se_type  = "CR2",
      clusters = datos[[cluster]]
    )
  )
  
  return(list(
    pred_wealth = pred_decile,
    formulas   = forms,
    models     = models
  ))
}

# 4. Descriptive ----------------------------------------------------------

# 4.2 Descriptive Decile land price by covariates ----

df_study1 <- df_study1 %>% 
  mutate(f_quintile_uf2018 = paste0("Q", quintile_uf2018),
         f_quintile_uf2018 = factor(f_quintile_uf2018,
                                  levels = c(
                                    "Q1",
                                    "Q2",
                                    "Q3",
                                    "Q4",
                                    "Q5")))

frq(df_study1$quintile_uf2018)
frq(df_study1$f_quintile_uf2018)

# Age

tab_wd_age <- bind_rows(
  df_study1 %>%
    group_by(f_quintile_uf2018) %>%
    summarise(
      mean    = mean(age, na.rm = TRUE),
      sd      = sd(age, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = "All")
  ,
  df_study1 %>%
    group_by(f_quintile_uf2018, ola) %>%
    summarise(
      mean    = mean(age, na.rm = TRUE),
      sd      = sd(age, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = as.factor(ola))
) %>% 
  arrange(ola) %>% 
  pivot_wider(id_cols = f_quintile_uf2018,
              names_from = ola,
              values_from = c(mean, sd)) %>% 
  mutate_if(is.numeric, ~ round(., 1))

years <- sub("^mean_", "", grep("^mean_", names(tab_wd_age), value = TRUE))

for (y in years) {
  tab_wd_age[[paste0("age_", y)]] <- paste0(tab_wd_age[[paste0("mean_", y)]], " (", tab_wd_age[[paste0("sd_", y)]], ")")
}

tab_wd_age <- tab_wd_age %>% select(f_quintile_uf2018, starts_with("age_"))

# Educ

tab_wd_educ <- bind_rows(
  df_study1 %>%
    group_by(f_quintile_uf2018) %>%
    summarise(
      mean    = mean(educyear, na.rm = TRUE),
      sd      = sd(educyear, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = "All")
  ,
  df_study1 %>%
    group_by(f_quintile_uf2018, ola) %>%
    summarise(
      mean    = mean(educyear, na.rm = TRUE),
      sd      = sd(educyear, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = as.factor(ola))
) %>% 
  arrange(ola) %>% 
  pivot_wider(id_cols = f_quintile_uf2018,
              names_from = ola,
              values_from = c(mean, sd)) %>% 
  mutate_if(is.numeric, ~ round(., 1))

years <- sub("^mean_", "", grep("^mean_", names(tab_wd_educ), value = TRUE))

for (y in years) {
  tab_wd_educ[[paste0("educ_", y)]] <- paste0(tab_wd_educ[[paste0("mean_", y)]], " (", tab_wd_educ[[paste0("sd_", y)]], ")")
}

tab_wd_educ <- tab_wd_educ %>% select(f_quintile_uf2018, starts_with("educ_"))

# Income

tab_wd_income <- bind_rows(
  df_study1 %>%
    group_by(f_quintile_uf2018) %>%
    summarise(
      mean    = mean(inc_eq_real_sqrt, na.rm = TRUE),
      sd      = sd(inc_eq_real_sqrt, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = "All")
  ,
  df_study1 %>%
    group_by(f_quintile_uf2018, ola) %>%
    summarise(
      mean    = mean(inc_eq_real_sqrt, na.rm = TRUE),
      sd      = sd(inc_eq_real_sqrt, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = as.factor(ola))
) %>% 
  arrange(ola) %>% 
  pivot_wider(id_cols = f_quintile_uf2018,
              names_from = ola,
              values_from = c(mean, sd)) %>% 
  mutate_if(is.numeric, ~ round(., 0))

tab_wd_income <- tab_wd_income %>% 
  mutate_if(is.numeric, ~ paste0("$", format(., big.mark = ",")))

years <- sub("^mean_", "", grep("^mean_", names(tab_wd_income), value = TRUE))

for (y in years) {
  tab_wd_income[[paste0("income_", y)]] <- paste0(tab_wd_income[[paste0("mean_", y)]], " (", tab_wd_income[[paste0("sd_", y)]], ")")
}

tab_wd_income <- tab_wd_income %>% select(f_quintile_uf2018, starts_with("income_"))

# Class (ISEI)

tab_wd_class <- bind_rows(
  df_study1 %>%
    group_by(f_quintile_uf2018) %>%
    summarise(
      mean    = mean(isei, na.rm = TRUE),
      sd      = sd(isei, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = "All")
  ,
  df_study1 %>%
    group_by(f_quintile_uf2018, ola) %>%
    summarise(
      mean    = mean(isei, na.rm = TRUE),
      sd      = sd(isei, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(ola = as.factor(ola))
) %>% 
  arrange(ola) %>% 
  pivot_wider(id_cols = f_quintile_uf2018,
              names_from = ola,
              values_from = c(mean, sd)) %>% 
  mutate_if(is.numeric, ~ round(., 1))

years <- sub("^mean_", "", grep("^mean_", names(tab_wd_class), value = TRUE))

for (y in years) {
  tab_wd_class[[paste0("isei_", y)]] <- paste0(tab_wd_class[[paste0("mean_", y)]], " (", tab_wd_class[[paste0("sd_", y)]], ")")
}

tab_wd_class <- tab_wd_class %>% select(f_quintile_uf2018, starts_with("isei_"))

# Housing

tab_wd_housing <- bind_rows(
  df_study1 %>% 
    group_by(housing) %>% 
    count(f_quintile_uf2018) %>% 
    pivot_wider(id_cols = housing,
                names_from = f_quintile_uf2018,
                values_from = n) %>% 
    mutate(ola = "All") %>% 
    ungroup(),
  df_study1 %>% 
    group_by(housing, ola) %>% 
    count(f_quintile_uf2018) %>% 
    pivot_wider(id_cols = c(housing, ola),
                names_from = f_quintile_uf2018,
                values_from = n) %>% 
    arrange(ola) %>% 
    mutate(ola = as.factor(ola)) %>% 
    ungroup()
) %>%
  mutate(
    total = rowSums(across(starts_with("Q")), na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("Q"), ~ .x / total, .names = "{.col}_prop")
  ) %>% 
  mutate(
    across(ends_with("prop"), ~ scales::percent(., 0.1))) %>% 
  mutate(
    across(starts_with("Q"), ~ (as.character(.)))) %>% 
  mutate(across(
    Q1:Q5,
    ~ paste0(.x, " (", get(paste0(cur_column(), "_prop")), ")")
  )) %>% 
  select(housing, ola, Q1:Q5) %>% 
  arrange(ola)

# 5. Pooled OLS regressions ---------------------------------------------------------

varsdep <- c(
  "identification", "friends", "size_network",
  "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
  "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
  "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)


## 5.1 New set of regression models: Social cohesion and housing-wealth extremes --------------
## Formula: DV ~ dummy_quintile_uf2018 + wave FE + age + controls (CR2 SE) 
## Sample: only 1 and 2 in housing (owners)

models_quintile <- map(varsdep, ~ estimate_lm_robust(.x, 
                                                     pred1="dummy_quintile_uf2018", 
                                                     pred2="ola",
                                                     pred3 ="age",
                                                     controls=c("educyear", "isei", "ln_inc_eq_real_sqrt"),  
                                                     datos=subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")))) %>% 
  set_names(varsdep)


# plot

df_dummy_quintile <- imap_dfr(models_quintile, ~{
  broom::tidy(.x[[5]], conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(outcome = .y)
}) %>% 
  filter(term %in% c("dummy_quintile_uf2018", "educyear", "isei", "ln_inc_eq_real_sqrt"))

df_dummy_quintile$high_lab <- if_else(df_dummy_quintile$p.value < 0.05, TRUE, FALSE)

df_dummy_quintile <- df_dummy_quintile %>% 
  mutate(term = case_when(term == "dummy_quintile_uf2018" ~ "Top 20% housing wealth",
                          term == "educyear" ~ "Education",
                          term == "isei" ~ "ISEI",
                          term == "ln_inc_eq_real_sqrt" ~ "Log equivalised household income"),
         term = factor(term, 
                       levels = c("Top 20% housing wealth", 
                                  "Education", 
                                  "ISEI", 
                                  "Log equivalised household income")),
         outcome = case_when(
           outcome == "identification" ~ "Cultural identification",
           outcome == "friends" ~"Number of friends",
           outcome == "size_network"~"Network size",
           outcome == "gen_trust"~"Generalized trust",
           outcome == "trust_minorities" ~"Trust in minorities",
           outcome == "trust_inst"~"Trust in major institutions",
           outcome == "interest_pol"~"Political engagement",
           outcome == "satisf_demo"~"Satisfaction with democracy",
           outcome == "conv_particip"~"Conventional political participation",
           outcome == "unconv_particip"~"Unconventional political participation",
           outcome == "egalitarianism"~"Egalitarianism",
           outcome == "altruistic"~"Altruistic dispositions",
           outcome == "prosoc_behave"~"Prosocial behavior",
           outcome == "democracy_support"~"Democracy support",
           outcome == "justif_violence"~"Justification of violence"
         ),
         outcome = factor(outcome,
                          levels = c(
                            "Cultural identification",
                            "Number of friends",
                            "Network size",
                            "Generalized trust",
                            "Trust in minorities",
                            "Trust in major institutions",
                            "Political engagement",
                            "Satisfaction with democracy",
                            "Conventional political participation",
                            "Unconventional political participation",
                            "Egalitarianism",
                            "Altruistic dispositions",
                            "Prosocial behavior",
                            "Democracy support",
                            "Justification of violence"
                          )))

gdummy_quintile <- df_dummy_quintile %>% 
  ggplot(aes(x = estimate, y = term, group = outcome)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_dummy_quintile %>% filter(high_lab),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  facet_wrap(~ outcome, scales = "fixed") +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2019 (N obs. = 1391, N clusters = 548)\nConfidence intervals (IC) at 95%\nEstimates in red are p<0.05")

# plot changes in coef

k_models <- 2:5
k_labels <- paste0("M", seq_along(k_models))  # M1..M4

df_quintile <- imap_dfr(models_quintile, \(mods, dv) {
  map_dfr(k_models, \(k) {
    tidy(mods[[k]], conf.int = TRUE) %>%
      filter(term == "dummy_quintile_uf2018") %>%
      mutate(
        outcome = dv,
        model = factor(paste0("M", match(k, k_models)), levels = k_labels)
      )
  })
})

df_quintile$high_lab <- if_else(df_quintile$p.value < 0.05, TRUE, FALSE)

df_quintile <- df_quintile %>% 
  mutate(termino = model,
         outcome = case_when(
           outcome == "identification" ~ "Cultural identification",
           outcome == "friends" ~"Number of friends",
           outcome == "size_network"~"Network size",
           outcome == "gen_trust"~"Generalized trust",
           outcome == "trust_minorities" ~"Trust in minorities",
           outcome == "trust_inst"~"Trust in major institutions",
           outcome == "interest_pol"~"Political engagement",
           outcome == "satisf_demo"~"Satisfaction with democracy",
           outcome == "conv_particip"~"Conventional political participation",
           outcome == "unconv_particip"~"Unconventional political participation",
           outcome == "egalitarianism"~"Egalitarianism",
           outcome == "altruistic"~"Altruistic dispositions",
           outcome == "prosoc_behave"~"Prosocial behavior",
           outcome == "democracy_support"~"Democracy support",
           outcome == "justif_violence"~"Justification of violence"
         ),
         outcome = factor(outcome,
                          levels = c(
                            "Cultural identification",
                            "Number of friends",
                            "Network size",
                            "Generalized trust",
                            "Trust in minorities",
                            "Trust in major institutions",
                            "Political engagement",
                            "Satisfaction with democracy",
                            "Conventional political participation",
                            "Unconventional political participation",
                            "Egalitarianism",
                            "Altruistic dispositions",
                            "Prosocial behavior",
                            "Democracy support",
                            "Justification of violence"
                          )))

# All

g_quintile_controls <- df_quintile %>% 
  ggplot(aes(x = estimate, y = termino, group = outcome)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_quintile %>% filter(high_lab),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  facet_wrap(~ outcome, scales = "fixed") +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: authors' elaboration using pooled ELSOC data (2016-2019; N = 1,391 person-waves; 548 respondents)\nError bars show 95% confidence intervals\nEstimates highlighted in red are statistically significant (p < .05)\nM1 includes housing wealth, wave fixed effects, and age; M2 adds education; M3 adds social class; M4 adds equivalized income")


# Cultural
g_cultural_quintile <- df_quintile %>% 
  filter(outcome == "Cultural identification") %>% 
  ggplot(aes(x = estimate, y = termino)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_quintile %>% filter(high_lab & outcome == "Cultural identification"),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: authors' elaboration using pooled ELSOC data (2016-2019; N = 1,391 person-waves; 548 respondents)\nError bars show 95% confidence intervals\nEstimates highlighted in red are statistically significant (p < .05)\nM1 includes housing wealth, wave fixed effects, and age; M2 adds education; M3 adds social class; M4 adds equivalized income")


# Relational
g_relational_quintile <- df_quintile %>% 
  filter(outcome %in% c("Number of friends",
                        "Network size",
                        "Generalized trust",
                        "Trust in minorities",
                        "Trust in major institutions")) %>% 
  ggplot(aes(x = estimate, y = termino, group = outcome)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_quintile %>% filter(high_lab & outcome %in% c("Number of friends",
                                                            "Network size",
                                                            "Generalized trust",
                                                            "Trust in minorities",
                                                            "Trust in major institutions")),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  facet_wrap(~ outcome, scales = "fixed") +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: authors' elaboration using pooled ELSOC data (2016-2019; N = 1,391 person-waves; 548 respondents)\nError bars show 95% confidence intervals\nEstimates highlighted in red are statistically significant (p < .05)\nM1 includes housing wealth, wave fixed effects, and age; M2 adds education; M3 adds social class; M4 adds equivalized income")

# Political
g_political_quintile <- df_quintile %>% 
  filter(outcome %in% c( "Political engagement",
                         "Satisfaction with democracy",
                         "Conventional political participation",
                         "Unconventional political participation",
                         "Egalitarianism",
                         "Altruistic dispositions",
                         "Prosocial behavior")) %>% 
  ggplot(aes(x = estimate, y = termino, group = outcome)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_quintile %>% filter(high_lab & outcome %in% c( "Political engagement",
                                                             "Satisfaction with democracy",
                                                             "Conventional political participation",
                                                             "Unconventional political participation",
                                                             "Egalitarianism",
                                                             "Altruistic dispositions",
                                                             "Prosocial behavior")),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  facet_wrap(~ outcome, scales = "fixed") +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: authors' elaboration using pooled ELSOC data (2016-2019; N = 1,391 person-waves; 548 respondents)\nError bars show 95% confidence intervals\nEstimates highlighted in red are statistically significant (p < .05)\nM1 includes housing wealth, wave fixed effects, and age; M2 adds education; M3 adds social class; M4 adds equivalized income")

# Normative

g_normative_quintile <- df_quintile %>% 
  filter(outcome %in% c("Democracy support",
                        "Justification of violence")) %>% 
  ggplot(aes(x = estimate, y = termino, group = outcome)) +
  geom_vline(xintercept = 0, linewidth = 0.7, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), fatten = 1.5, 
                  size = 1, color = "#000004FF") +
  geom_pointrange(
    data = df_quintile %>% filter(high_lab & outcome %in% c("Democracy support",
                                                            "Justification of violence")),
    aes(xmin = conf.low, xmax = conf.high),
    fatten = 1.5, size = 1, color = "#ca1137"
  ) +
  facet_wrap(~ outcome, scales = "fixed") +
  labs(x = "Estimate",
       y = NULL,
       caption = "Source: authors' elaboration using pooled ELSOC data (2016-2019; N = 1,391 person-waves; 548 respondents)\nError bars show 95% confidence intervals\nEstimates highlighted in red are statistically significant (p < .05)\nM1 includes housing wealth, wave fixed effects, and age; M2 adds education; M3 adds social class; M4 adds equivalized income")


## 5.2 Interaction set of regression models --------------
## Formula: DV ~ dummy_quiuntile_uf2018*housing + wave FE + age + controls (CR2 SE) 
## Sample: only 1 y 2 in housing owners, drop other

res_interactions <- map(varsdep, ~ estimate_lm_robust_interact2sets(
  vardep = .x, 
  housing = "housing",                 
  pred_price = "ln_uf2018",            
  pred_decile = "dummy_quintile_uf2018", 
  wave = "ola",                        
  age = "age",
  cluster = "idencuesta",
  controls = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
  datos = df_int)) %>% 
  set_names(varsdep)

models_int_lnprice <- map(res_interactions, "set_price")
models_int_dummy_quintile <- map(res_interactions, "set_decile")

# plots int dummy quintile

dv_names <- names(models_int_dummy_quintile)

m_ref <- models_int_dummy_quintile[[dv_names[1]]]$models[[5]]
housing_levels <- levels(model.frame(m_ref)$housing)

my_cols <- setNames(
  c("#DE4968FF", "#3B0F70FF")[seq_along(housing_levels)],
  housing_levels
)

legend_title <- "Homeownership"

pretty_titles <- c(
  "Cultural identification",
  "Number of friends",
  "Network size",
  "Generalized trust",
  "Trust in minorities",
  "Trust in major institutions",
  "Political engagement",
  "Satisfaction with democracy",
  "Conventional political participation",
  "Unconventional political participation",
  "Egalitarianism",
  "Altruistic dispositions",
  "Prosocial behavior",
  "Democracy support",
  "Justification of violence"
)

dv_title_map <- setNames(pretty_titles, dv_names)

plots_m6 <- map(dv_names, \(dv) {
  m <- models_int_dummy_quintile[[dv]]$models[[5]]
  
  plot_predictions(
    m,
    condition = c("dummy_quintile_uf2018", "housing"),
    vcov = vcov(m)
  ) +
    labs(
      title = dv_title_map[[dv]],
      x = "Top 20% housing wealth",
      y = "Predicted value"
    ) +
    # colores + mismo nombre de leyenda
    scale_color_manual(values = my_cols, breaks = housing_levels, name = legend_title) +
    scale_fill_manual(values  = my_cols, breaks = housing_levels, name = legend_title) +
    # una sola leyenda (quita la de fill)
    guides(fill = "none",
           color = guide_legend(override.aes = list(fill = NA)))})

names(plots_m6) <- dv_names


# 6. Save models ----------------------------------------------------------

save(models_quintile,
     models_int_dummy_quintile, file = here("output/models/pooled_ols_models_V4.RData"))
