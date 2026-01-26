#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analysis code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to perform regression analysis
# Date: December 22, 2025
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
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

# 2. Data -----------------------------------------------------------------

load(here("output/data/df_study1.RData"))

glimpse(df_study1)

# convert wave to factor for FE
df_study1 <- df_study1 %>% 
  mutate(ola = factor(ola, levels = c("2016", "2017", "2018", "2019")),
         ola = sjlabelled::set_label(ola, "Wave"),
         housing = factor(housing, 
                          levels = c("Other regime", 
                                     "Owned and fully paid-off home",
                                     "Owned home with mortgage payments",
                                     "Rented housing")))

# 3. Function -------------------------------------------------------------

estimate_lm_robust <- function(
    vardep, pred1 = "housing", pred2 = "ola", cluster = "idencuesta",
    controls = c("cine", "class", "decile_eq"),
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
    glue("{vardep} ~ {pred1} + {pred2} + {controls[1]}"),
    glue("{vardep} ~ {pred1} + {pred2} + {controls[1]} + {controls[2]}"),
    glue("{vardep} ~ {pred1} + {pred2} + {controls[1]} + {controls[2]} + {controls[3]}")
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
## Formula: DV ~ housing + wave FE + controls (CR2 SE)

varsdep <- c(
  "identification", "friends", "size_network",
  "gen_trust", "trust_minorities", "trust_inst", "interest_pol",
  "satisf_demo", "conv_particip", "unconv_particip", "egalitarianism",
  "altruistic", "prosoc_behave", "democracy_support", "justif_violence"
)


models1 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="housing", 
                                             pred2="ola",
                                             controls=c("cine","class","decile_eq"), 
                                             datos=df_study1)) %>% 
  set_names(varsdep)
  

lm_robust(identification ~ housing + ola + cine,
          se_type = "CR2",
          clusters = idencuesta,
          data = df_study1) # ok!


## Second set of regression models --------------
## Formula: DV ~ ln_uf2018 + wave FE + controls (CR2 SE) 
## Sample: only 1 and 2 in housing (owners)

models2 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="ln_uf2018", 
                                             pred2="ola",
                                             controls=c("cine","class","decile_eq"), 
                                             datos=subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")))) %>% 
  set_names(varsdep)


## Third set of regression models --------------
## Formula: DV ~ dummy_decile_uf2018 + wave FE + controls (CR2 SE) 
## Sample: only 1 and 2 in housing (owners)

models3 <- map(varsdep, ~ estimate_lm_robust(.x, 
                                             pred1="dummy_decile_uf2018", 
                                             pred2="ola",
                                             controls=c("cine","class","decile_eq"), 
                                             datos=subset(df_study1, housing %in% c("Owned and fully paid-off home", "Owned home with mortgage payments")))) %>% 
  set_names(varsdep)



# 4. Save models ----------------------------------------------------------

save(models1, models2, models3, file = here("output/models/pooled_ols_models.RData"))

screenreg(models3$egalitarianism[[1]])
