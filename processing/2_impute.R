#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Imputing values and transforming ciuo
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 2.1 Transform CIUO ---------------------------------------------------------------------------------------------------------------------

# Reduce CIUO crosswalk (keep one-to-one mapping by ciuo88)
insumo_ciuo_reduced <- insumo_ciuo %>%
  arrange(ciuo88) %>%
  distinct(ciuo88, .keep_all = TRUE)

# Join variables
db <- db %>%
  left_join(
    insumo_ciuo_reduced %>% select(ciuo88, ciuo08) %>% rename(ciuo08_from88_m03 = ciuo08),
    by = c("ciuo88_m03" = "ciuo88")
  ) %>%
  left_join(
    insumo_ciuo_reduced %>% select(ciuo88, ciuo08) %>% rename(ciuo08_from88_m22 = ciuo08),
    by = c("ciuo88_m22" = "ciuo88")
  ) %>%
  mutate(
    ciuo08_m03 = coalesce(ciuo08_m03, ciuo08_from88_m03),
    ciuo08_m22 = coalesce(ciuo08_m22, ciuo08_from88_m22)
  ) %>%
  select(-ciuo08_from88_m03, -ciuo08_from88_m22)



# 2.2 Manual imputation of main variables ----------------------------------------------------------------------------------

# Transform to wide for imputation and interpolation
db_wide <- db %>%
  mutate(
    wave = case_when(
      ola == 2016 ~ "w01",
      ola == 2017 ~ "w02",
      ola == 2018 ~ "w03",
      ola == 2019 ~ "w04",
      ola == 2021 ~ "w05",
      ola == 2022 ~ "w06",
      ola == 2023 ~ "w07",
      TRUE ~ NA_character_
    )
  ) %>%
  pivot_wider(
    id_cols = c(idencuesta, muestra),
    names_from = wave,
    values_from = -c(idencuesta, ola, wave, muestra),
  )

# DEPENDENT VARIABLES: Create variables that doesn't have measurement in some waves

## Network variables (r15 and r13_*)


db_wide <- db_wide %>%
  mutate(
    # ------------------------------------------------------------
    # Overwrite only in wave 1 where r15 was not asked with value in wave 2
    # ------------------------------------------------------------
    r15_w01 = r15_w02,
    r13_nredes_w01 = r13_nredes_w02,
    rec_r13_nredes_w01 = rec_r13_nredes_w02
    )

## c06_

db_wide <- db_wide %>%
  mutate(
    # ------------------------------------------------------------
    # c06_04: asked in 2016, 2018, 2022 only (all NA in 2017, 2019, 2021, 2023)
    # Rule: interpolate when bracketed; otherwise nearest available wave
    # ------------------------------------------------------------
    c06_04_w04 = (c06_04_w03 + c06_04_w06) / 2,
    # ------------------------------------------------------------
    # c06_05: asked in 2016, 2018, 2022, 2023 (all NA in 2017, 2019, 2021)
    # Rule: interpolate when bracketed
    # ------------------------------------------------------------
    c06_05_w04 = (c06_05_w03 + c06_05_w06) / 2,
    # ------------------------------------------------------------
    # c06_06: asked in 2016, 2018, 2022, 2023 (all NA in 2017, 2019, 2021)
    # Rule: interpolate when bracketed
    # ------------------------------------------------------------
    c06_06_w04 = (c06_06_w03 + c06_06_w06) / 2
    )


# c12_

db_wide <- db_wide %>%
  mutate(
    # ------------------------------------------------------------
    # c12_*: asked in 2016, 2018, 2022, 2023 (all NA in 2017, 2019, 2021)
    # Rule: interpolate when bracketed; otherwise nearest available wave
    # ------------------------------------------------------------
    c12_01_w04 = (c12_01_w03 + c12_01_w06) / 2,
    c12_03_w04 = (c12_03_w03 + c12_03_w06) / 2,
    c12_04_w04 = (c12_04_w03 + c12_04_w06) / 2,
    c12_05_w04 = (c12_05_w03 + c12_05_w06) / 2
  )

# c18_

db_wide <- db_wide %>%
  mutate(
    # ------------------------------------------------------------
    # c18_02 and c18_03: asked in 2016/2017/2018/2019 and 2023
    # Not asked in 2021 and 2022 (all NA)
    # Rule: impute closer wave
    # ------------------------------------------------------------
    c18_02_w05 = c18_02_w04,
    c18_02_w06 = c18_02_w07,
    c18_03_w05 = c18_03_w04,
    c18_03_w06 = c18_03_w07
  )

# c07_

db_wide <- db_wide %>%
  mutate(
    # ------------------------------------------------------------
    # c07_04 and c07_05: asked in 2016/2017/2018/2019/2021 and 2023
    # Not asked in 2022 (all NA)
    # Rule: impute closer wave 2021
    # ------------------------------------------------------------
    c07_04_w06 = c07_04_w05,
    c07_05_w06 = c07_05_w05
  )

# INDEPENDENT VARIABLES

db_wide <- db_wide %>% 
  mutate(
    # ------------------------------------------------------------
    # m33: asked in 2016/2018/2021 and 2023
    # Not asked in 2017/2019/2022 (all NA)
    # Rule: impute last closer wave
    # ------------------------------------------------------------
  m33_w02 = m33_w01,
  m33_w04 = m33_w03,
  m33_w06 = m33_w05
    )


db_wide <- db_wide %>% 
  mutate(
    # ------------------------------------------------------------
    # ciuo08: asked in 2016/2018/2021 and 2023
    # Not asked in 2017/2019/2022 (all NA)
    # Rule: impute last closer wave
    # ------------------------------------------------------------
    ciuo08_m03_w02 = ciuo08_m03_w01,
    ciuo08_m03_w04 = ciuo08_m03_w03,
    ciuo08_m03_w06 = ciuo08_m03_w05
  )


db_wide <- db_wide %>% 
  mutate(
    # ------------------------------------------------------------
    # ciuo08_m22: asked in 2016/2018/2021 and 2023
    # Not asked in 2017/2019/2022 (all NA)
    # Rule: impute last closer wave
    # ------------------------------------------------------------
    ciuo08_m22_w02 = ciuo08_m22_w01,
    ciuo08_m22_w04 = ciuo08_m22_w03,
    ciuo08_m22_w06 = ciuo08_m22_w05,
    # ------------------------------------------------------------
    # m21: asked in 2016/2018/2021 and 2023
    # Not asked in 2017/2019/2022 (all NA)
    # Rule: impute last closer wave
    # ------------------------------------------------------------
    m21_w02 = m21_w01,
    m21_w04 = m21_w03,
    m21_w06 = m21_w05,
  )

db_wide <- db_wide %>% 
  mutate(
    # Imputate ciuo for retired and unemployed
    # Interview
    ciuo08_m03_w01 = if_else(is.na(ciuo08_m03_w01) & m02_w01 == 5, 15000, ciuo08_m03_w01), # retired
    ciuo08_m03_w01 = if_else(is.na(ciuo08_m03_w01) & m02_w01 == 6, 16000, ciuo08_m03_w01), # unemployed
    ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 5, 15000, ciuo08_m03_w04),
    ciuo08_m03_w04 = if_else(is.na(ciuo08_m03_w04) & m02_w04 == 6, 16000, ciuo08_m03_w04),
    ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 5, 15000, ciuo08_m03_w06),
    ciuo08_m03_w06 = if_else(is.na(ciuo08_m03_w06) & m02_w06 == 6, 16000, ciuo08_m03_w06),
    # Household sostainer
    ciuo08_m22_w01 = if_else(is.na(ciuo08_m22_w01) & m21_w01 == 5, 15000, ciuo08_m22_w01), # retired
    ciuo08_m22_w01 = if_else(is.na(ciuo08_m22_w01) & m21_w01 == 6, 16000, ciuo08_m22_w01), # unemployed
    ciuo08_m22_w04 = if_else(is.na(ciuo08_m22_w04) & m21_w04 == 5, 15000, ciuo08_m22_w04),
    ciuo08_m22_w04 = if_else(is.na(ciuo08_m22_w04) & m21_w04 == 6, 16000, ciuo08_m22_w04),
    ciuo08_m22_w06 = if_else(is.na(ciuo08_m22_w06) & m21_w06 == 5, 15000, ciuo08_m22_w06),
    ciuo08_m22_w06 = if_else(is.na(ciuo08_m22_w06) & m21_w06 == 6, 16000, ciuo08_m22_w06)
  )


# 2.2 Manual imputation of values for occupation --------------------------------------------------------------------------------------------------------------------

db_wide <- db_wide %>%
  # Imputation for WAVE 1
  mutate(
    # for wave 1: inactive and unemployed
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w01), # assign household head occupation to inactive (student, domestic, disability, nini)
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w03), # assign individual occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w03), # assign household head occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w05), # assign individual occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m22_w05) # assign household head occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
  ) %>%
  # Imputation for WAVE 4
  mutate(
    # Inactive and unemployed
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w03),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w01),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w01),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w05),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m22_w05)
  ) %>%
  # Imputation for WAVE 6
  mutate(
    # Inactive and unemployed
    ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w05),
    ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w04),
    ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w03),
    ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m03_w01),
    ciuo08_m03_w06 = coalesce(ciuo08_m03_w06, ciuo08_m22_w01)
  ) %>%
  # For the remaining missing values, we imput the values of 2016
  mutate(
    ciuo08_m03_w01 = coalesce(ciuo08_m03_w01, ciuo08_m03_w06),
    ciuo08_m03_w04 = coalesce(ciuo08_m03_w04, ciuo08_m03_w06)
  )

# 2.3 Automatic imputations for dependent variables -----------------------------------------------------------------------------------------------------
db %>%
  group_by(ola) %>%
  summarise(
    n_rows   = n(),
    n_non_na = sum(!is.na(m07)),
    all_na   = (n_non_na == 0),
    .groups  = "drop"
  ) %>%
  arrange(ola)
# Create vectors with variables to impute
vars_to_impute <- c(
  "r15", "r13_nredes", "rec_r13_nredes",
  "c06_04", "c06_05", "c06_06",
  "c12_01", "c12_03", "c12_04", "c12_05",
  "c08_01", "c08_03",
  "d02_01", "d02_02", "d02_03",
  "c18_02", "c18_03",
  "c07_04", "c07_05",
  "f05_01", "f05_02",
  "m33", "ciuo08_m03", "ciuo08_m22", "m06", "m07" #* INDEPENDENT VARIABLES
)

# Impute values for w01 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06", "w07"))
  },
  .init = db_wide
)

# Impute values for w02 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w02", waves_source = c("w01", "w03", "w04", "w05", "w06", "w07"))
  },
  .init = db_wide
)

# Impute values for w03 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w03", waves_source = c("w02", "w04", "w01", "w05", "w06", "w07"))
  },
  .init = db_wide
)

# Impute values for w04 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w04", waves_source = c("w03", "w05", "w02", "w06", "w01", "w07"))
  },
  .init = db_wide
)

# Impute values for w05 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w05", waves_source = c("w04", "w06", "w03", "w07", "w02", "w01"))
  },
  .init = db_wide
)

# Impute values for w06 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w06", waves_source = c("w05", "w07", "w04", "w03", "w02", "w01"))
  },
  .init = db_wide
)

# Impute values for w07 variables
db_wide <- reduce(
  vars_to_impute,
  function(df, var) {
    impute_waves(df, var, wave_to_impute = "w07", waves_source = c("w06", "w05", "w04", "w03", "w02", "w01"))
  },
  .init = db_wide
)

# Remove objects from the global enviroment
rm(vars_to_impute, insumo_ciuo, insumo_ciuo_reduced)


