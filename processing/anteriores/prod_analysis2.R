#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analysis code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to perform regression analysis
# Date: January 7, 2026
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

# 2. Data -----------------------------------------------------------------

load(here("output/data/df_study1_V2.RData"))
load(here("output/data/db_long_V2.RData"))

glimpse(df_study1)

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
    vardep, pred1 = "housing", pred2 = "ola", pred3 = "age", cluster = "idencuesta",
    controls = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
    datos = df_study1, transform = FALSE, relevel = FALSE, relevel_cat = 3
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

estimate_lm_robust_interact4 <- function(
    vardep,
    pred1 = "housing",      # homeownership (factor con 4 categorías)
    pred2 = "ln_uf2018",    # log land price
    pred3 = "ola",          # wave FE
    pred4 = "age",          # edad
    cluster = "idencuesta",
    controls = c("educyear", "isei", "ln_inc_eq_real_sqrt"),
    datos = df_study1,
    transform = FALSE,
    relevel_home = FALSE,
    ref_home = NULL         # si lo usas, pierdes "4 slopes directos" (pasa a referencia)
) {
  
  # Transform pred1 to factor/labels if necessary
  if (transform) {
    datos[[pred1]] <- to_label(datos[[pred1]])
  }
  datos[[pred1]] <- as.factor(datos[[pred1]])
  
  # Relevel pred1 if necessary (NO recomendado si quieres 4 slopes directos)
  if (relevel_home && !is.null(ref_home)) {
    datos[[pred1]] <- relevel(datos[[pred1]], ref = ref_home)
  }
  
  # Base term: 0 + housing + ln_uf2018:housing + factor(ola)
  # -> sin intercepto global
  # -> incluye dummies de housing
  # -> incluye 1 pendiente por categoría (interacción)
  # -> NO incluye ln_uf2018 solo
  base_terms <- glue("0 + {pred1} + {pred2}:{pred1} + {pred3}")
  
  # Modelos anidados (igual que tu estructura previa)
  forms <- c(
    glue("{vardep} ~ {base_terms}"),
    glue("{vardep} ~ {base_terms} + {pred4}"),
    glue("{vardep} ~ {base_terms} + {pred4} + {controls[1]}"),
    glue("{vardep} ~ {base_terms} + {pred4} + {controls[1]} + {controls[2]}"),
    glue("{vardep} ~ {base_terms} + {pred4} + {controls[1]} + {controls[2]} + {controls[3]}")
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


# 4. Run analysis ---------------------------------------------------------

## First set of regression models --------------
## Formula: DV ~ housing + wave FE + age + controls (CR2 SE)

varsdep <- c(
  "identification", "friends", "size_network",
  "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
  "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
  "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)


models1 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="housing", 
                                             pred2="ola",
                                             pred3 ="age",
                                             controls=c("educyear", "isei", "ln_inc_eq_real_sqrt"), 
                                             datos=df_study1)) %>% 
  set_names(varsdep)

screenreg(models1$identification[[5]])

lm_robust(identification ~ housing + ola + age + educyear + isei + 
            ln_inc_eq_real_sqrt, 
          se_type = "CR2",
          clusters = idencuesta,
          data = df_study1) %>% 
  screenreg() # ok!


## Second set of regression models --------------
## Formula: DV ~ ln_uf2018 + wave FE + age + controls (CR2 SE) 
## Sample: only 1 and 2 in housing (owners)

models2 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="ln_uf2018", 
                                             pred2="ola",
                                             pred3 ="age",
                                             controls=c("educyear", "isei", "ln_inc_eq_real_sqrt"), 
                                             datos=subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")))) %>% 
  set_names(varsdep)


## Third set of regression models --------------
## Formula: DV ~ dummy_decile_uf2018 + wave FE + age + controls (CR2 SE) 
## Sample: only 1 and 2 in housing (owners)

models3 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="dummy_decile_uf2018", 
                                             pred2="ola",
                                             pred3 ="age",
                                             controls=c("educyear", "isei", "ln_inc_eq_real_sqrt"),  
                                             datos=subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")))) %>% 
  set_names(varsdep)



## Four set of regression models --------------
## Formula: DV ~ ln_uf2018*housing + wave FE + age + controls (CR2 SE) 
## Sample: all

models4 <- map(varsdep, ~ estimate_lm_robust_interact4(
                                             vardep = .x, 
                                             pred1  = "housing",
                                             pred2  = "ln_uf2018",
                                             pred3  = "ola",
                                             pred4  = "age",
                                             controls=c("educyear", "isei", "ln_inc_eq_real_sqrt"), 
                                             datos=df_study1)) %>% 
  set_names(varsdep)


# 4. Save models ----------------------------------------------------------

save(models1, models2, models3, models4, file = here("output/models/pooled_ols_models_V2.RData"))

# 5. Homeownership descriptive --------------------------------------------------

# Age

tab_age <- 
  bind_rows(
  df_study1 %>%
  group_by(housing) %>%
  summarise(
    n       = sum(!is.na(age)),
    mean    = mean(age, na.rm = TRUE),
    sd      = sd(age, na.rm = TRUE),
    median  = median(age, na.rm = TRUE),
    trimmed = mean(age, trim = .10, na.rm = TRUE),
    min     = min(age, na.rm = TRUE),
    max     = max(age, na.rm = TRUE),
    range   = max - min,
    skew    = psych::skew(age, na.rm = TRUE),
    kurtosis= psych::kurtosi(age, na.rm = TRUE),
    se      = sd / sqrt(n),
    Q0.25   = quantile(age, .25, na.rm = TRUE)[[1]],
    Q0.75   = quantile(age, .75, na.rm = TRUE)[[1]],
    .groups = "drop"
  ) %>% 
    mutate(ola = "All")
  ,
 df_study1 %>%
  group_by(housing, ola) %>%
  summarise(
    n       = sum(!is.na(age)),
    mean    = mean(age, na.rm = TRUE),
    sd      = sd(age, na.rm = TRUE),
    median  = median(age, na.rm = TRUE),
    trimmed = mean(age, trim = .10, na.rm = TRUE),
    min     = min(age, na.rm = TRUE),
    max     = max(age, na.rm = TRUE),
    range   = max - min,
    skew    = psych::skew(age, na.rm = TRUE),
    kurtosis= psych::kurtosi(age, na.rm = TRUE),
    se      = sd / sqrt(n),
    Q0.25   = quantile(age, .25, na.rm = TRUE)[[1]],
    Q0.75   = quantile(age, .75, na.rm = TRUE)[[1]],
    .groups = "drop"
  ) 
  ) %>% 
  arrange(ola)

tab_age

# Education


tab_educ <- 
  bind_rows(
    df_study1 %>%
      group_by(housing) %>%
      summarise(
        n       = sum(!is.na(educyear)),
        mean    = mean(educyear, na.rm = TRUE),
        sd      = sd(educyear, na.rm = TRUE),
        median  = median(educyear, na.rm = TRUE),
        trimmed = mean(educyear, trim = .10, na.rm = TRUE),
        min     = min(educyear, na.rm = TRUE),
        max     = max(educyear, na.rm = TRUE),
        range   = max - min,
        skew    = psych::skew(educyear, na.rm = TRUE),
        kurtosis= psych::kurtosi(educyear, na.rm = TRUE),
        se      = sd / sqrt(n),
        Q0.25   = quantile(educyear, .25, na.rm = TRUE)[[1]],
        Q0.75   = quantile(educyear, .75, na.rm = TRUE)[[1]],
        .groups = "drop"
      ) %>% 
      mutate(ola = "All")
    ,
    df_study1 %>%
      group_by(housing, ola) %>%
      summarise(
        n       = sum(!is.na(educyear)),
        mean    = mean(educyear, na.rm = TRUE),
        sd      = sd(educyear, na.rm = TRUE),
        median  = median(educyear, na.rm = TRUE),
        trimmed = mean(educyear, trim = .10, na.rm = TRUE),
        min     = min(educyear, na.rm = TRUE),
        max     = max(educyear, na.rm = TRUE),
        range   = max - min,
        skew    = psych::skew(educyear, na.rm = TRUE),
        kurtosis= psych::kurtosi(educyear, na.rm = TRUE),
        se      = sd / sqrt(n),
        Q0.25   = quantile(educyear, .25, na.rm = TRUE)[[1]],
        Q0.75   = quantile(educyear, .75, na.rm = TRUE)[[1]],
        .groups = "drop"
      ) 
  ) %>% 
  arrange(ola)

tab_educ

# Income

tab_income <- 
  bind_rows(
    df_study1 %>%
      group_by(housing) %>%
      summarise(
        n       = sum(!is.na(inc_eq_real_sqrt)),
        mean    = mean(inc_eq_real_sqrt, na.rm = TRUE),
        sd      = sd(inc_eq_real_sqrt, na.rm = TRUE),
        median  = median(inc_eq_real_sqrt, na.rm = TRUE),
        trimmed = mean(inc_eq_real_sqrt, trim = .10, na.rm = TRUE),
        min     = min(inc_eq_real_sqrt, na.rm = TRUE),
        max     = max(inc_eq_real_sqrt, na.rm = TRUE),
        range   = max - min,
        skew    = psych::skew(inc_eq_real_sqrt, na.rm = TRUE),
        kurtosis= psych::kurtosi(inc_eq_real_sqrt, na.rm = TRUE),
        se      = sd / sqrt(n),
        Q0.25   = quantile(inc_eq_real_sqrt, .25, na.rm = TRUE)[[1]],
        Q0.75   = quantile(inc_eq_real_sqrt, .75, na.rm = TRUE)[[1]],
        .groups = "drop"
      ) %>% 
      mutate(ola = "All")
    ,
    df_study1 %>%
      group_by(housing, ola) %>%
      summarise(
        n       = sum(!is.na(inc_eq_real_sqrt)),
        mean    = mean(inc_eq_real_sqrt, na.rm = TRUE),
        sd      = sd(inc_eq_real_sqrt, na.rm = TRUE),
        median  = median(inc_eq_real_sqrt, na.rm = TRUE),
        trimmed = mean(inc_eq_real_sqrt, trim = .10, na.rm = TRUE),
        min     = min(inc_eq_real_sqrt, na.rm = TRUE),
        max     = max(inc_eq_real_sqrt, na.rm = TRUE),
        range   = max - min,
        skew    = psych::skew(inc_eq_real_sqrt, na.rm = TRUE),
        kurtosis= psych::kurtosi(inc_eq_real_sqrt, na.rm = TRUE),
        se      = sd / sqrt(n),
        Q0.25   = quantile(inc_eq_real_sqrt, .25, na.rm = TRUE)[[1]],
        Q0.75   = quantile(inc_eq_real_sqrt, .75, na.rm = TRUE)[[1]],
        .groups = "drop"
      ) 
  ) %>% 
  arrange(ola)

tab_income

# Decile

tab_decile <- bind_rows(
  df_study1 %>% 
    group_by(housing) %>% 
    count(decile_eq) %>% 
    pivot_wider(id_cols = housing,
                names_from = decile_eq,
                values_from = n,
                names_prefix = "D") %>% 
    mutate(ola = "All") %>% 
    ungroup(),
  df_study1 %>% 
    group_by(housing, ola) %>% 
    count(decile_eq) %>% 
    pivot_wider(id_cols = c(housing, ola),
                names_from = decile_eq,
                values_from = n,
                names_prefix = "D") %>% 
    arrange(ola) %>% 
    ungroup()
) %>%
  mutate(
    total = rowSums(across(starts_with("D")), na.rm = TRUE)
  ) %>%
  mutate(
    across(starts_with("D"), ~ .x / total, .names = "{.col}_prop")
  ) %>% 
  mutate(
    across(ends_with("prop"), ~ scales::percent(., 0.1))) %>% 
  mutate(
    across(starts_with("D"), ~ (as.character(.)))) %>% 
  mutate(across(
    D1:D10,
    ~ paste0(.x, " (", get(paste0(cur_column(), "_prop")), ")")
  )) %>% 
  select(housing, ola, D1:D10) %>% 
  arrange(ola)


## Descriptive for housing

library(shadowtext)
library(ggdist)

datos.housing <- df_study1 %>% 
  group_by(idencuesta, ola) %>% 
  count(housing) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit()

etiquetas.housing <- df_study1 %>%
  group_by(ola, housing) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(idencuesta = 1)

g1 <- datos.housing %>% 
  ggplot(aes(x = ola, fill = housing, stratum = housing,
             alluvium = idencuesta, y = porcentaje)) +
  ggalluvial::geom_flow(alpha = .4) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values =  c("#DE4968FF","#8C2981FF","#3B0F70FF", "#FDB42FFF")) +
  geom_shadowtext(data = etiquetas.housing,
                  aes(label = ifelse(porcentaje > 0 , scales::percent(porcentaje, accuracy = .1),"")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = rep('white'),
                  bg.colour='grey30')+
  labs(y = "%",
       x = NULL,
       fill = NULL,
       title = NULL,
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2019 (N obs = 2,191; N individuals = 823)")+
  theme_ggdist() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) 

df_study1 %>% 
  select(idencuesta, ola, housing) %>% 
  drop_na() %>% 
  distinct(idencuesta)

datos.housing2 <- df_study1 %>% 
  dplyr::filter(ola %in% c(2016, 2019)) %>% 
  group_by(idencuesta, ola) %>% 
  count(housing) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(ola = factor(ola, levels = c("2016",
                                      "2019")))


etiquetas.housing2 <- df_study1 %>%
  dplyr::filter(ola %in% c(2016, 2019)) %>% 
  group_by(ola, housing) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(idencuesta = 1,
         ola = factor(ola, levels = c("2016",
                                      "2019")))


g2 <- datos.housing2 %>% 
  ggplot(aes(x = ola, fill = housing, stratum = housing,
             alluvium = idencuesta, y = porcentaje)) +
  ggalluvial::geom_flow(alpha = .4) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values =  c("#DE4968FF","#8C2981FF","#3B0F70FF", "#FDB42FFF")) +
  geom_shadowtext(data = etiquetas.housing2,
                  aes(label = ifelse(porcentaje > 0 , scales::percent(porcentaje, accuracy = .1),"")),
                  position = position_stack(vjust = .5),
                  show.legend = FALSE,
                  size = 4,
                  color = rep('white'),
                  bg.colour='grey30')+
  labs(y = "%",
       x = NULL,
       fill = NULL,
       title = NULL,
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2019 (N obs = 2,191; N individuals = 823)")+
  theme_ggdist() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) 

