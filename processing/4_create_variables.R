#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Recode and transform variables
# Responsable: Technical assistant
#******************************************************************************************************************************************************



# 4.1 Create dependent variables of social cohesion --------------------------------------------------------------------------------------------------------------------------

db_long <- db_long %>%
    mutate(
      #***** Cultural dimension
      identification = (c32_01 + c32_02) / 2,
      
      #***** Relational dimension
      friends = r15,
      size_network = r13_nredes,
      size_network_rec = rec_r13_nredes,
      gen_trust = c02,
      trust_minorities = (c06_04 + c06_05 + c06_06) / 3,
      trust_inst = (c05_01 + c05_02 + c05_05 + c05_07) / 4,
      
      #***** Political dimension
      interest_pol = c13,
      satisf_demo = c01,
      conv_particip = (c12_01 + c12_03 + c12_04 + c12_05) / 4,
      unconv_particip = (c08_01 + c08_02 + c08_03) / 3,
      egalitarianism = (d02_01 + d02_02 + d02_03) / 3,
      altruistic = (c18_02 + c18_03) / 2,
      prosoc_behave = (c07_04 + c07_05) / 2,
      
      #***** Normative dimension
      democracy_support = c25,
      justif_violence = (f05_01 + f05_02 + f05_03) / 3
    )

# 4.2 Create housing wealth variables --------------------------------------------------------------------------------------------------------------------------

# Home Ownership
frq(db_long$m33)

db_long <- db_long %>% 
  mutate(
    # Detailed recode and factor ordering
    m33_rec = case_when(
      m33 == 1 ~ "Owned and fully paid-off home",
      m33 == 2 ~ "Owned home with mortgage payments",
      m33 == 3 ~ "A rented home",
      m33 == 4 ~ "A home provided by work or service",
      m33 == 5 ~ "Owned home shared with one or more families",
      m33 == 6 ~ "Home provided by a family member or friend",
      m33 == 7 ~ "Other, please specify",
      TRUE ~ NA_character_
    ),
    m33_rec = factor(
      m33_rec,
      levels = c(
        "Owned and fully paid-off home",
        "Owned home with mortgage payments",
        "A rented home",
        "A home provided by work or service",
        "Owned home shared with one or more families",
        "Home provided by a family member or friend",
        "Other, please specify"
      )
    ),
    
    # Aggregated recode derived from m33_rec 
    housing = case_when(
      m33_rec %in% c(
        "Owned and fully paid-off home",
        "Owned home with mortgage payments"
      ) ~ m33_rec,
      m33_rec == "A rented home" ~ "Rented housing",
      m33_rec %in% c(
        "A home provided by work or service",
        "Owned home shared with one or more families",
        "Home provided by a family member or friend",
        "Other, please specify"
      ) ~ "Other regime",
      TRUE ~ NA_character_
    ),
    housing = factor(
      housing,
      levels = c(
        "Owned and fully paid-off home",
        "Owned home with mortgage payments",
        "Rented housing",
        "Other regime"
      )
    )
  )

frq(db_long$m33_rec)
frq(db_long$housing)

# Housing value m2
frq(db_long$uf2018)

db_long$ln_uf2018 <- log(db_long$uf2018)# Create log

db_long <- db_long %>%
  group_by(ola, comuna_cod) %>% # nivel de comuna, no manzana, ya que hay muy pocos casos por manzana en cada ola, a veces solo 1 caso incluso.
  mutate(
    area_n_uf  = sum(!is.na(uf2018)),
    area_sd_uf = if_else(area_n_uf >= 2, sd(uf2018, na.rm = TRUE), NA_real_)
  ) %>%
  ungroup()# create SD

frq(db_long$area_sd_uf)


frq(db_long$decile_uf2018)
frq(db_long$quintile_uf2018)

## Dummy for decil 10
db_long$dummy_decile_uf2018 <- if_else(db_long$decile_uf2018 == 10, 1, 0)

frq(db_long$dummy_decile_uf2018)

## Dummy for quintile 5
db_long$dummy_quintile_uf2018 <- if_else(db_long$quintile_uf2018 == 5, 1, 0)

frq(db_long$dummy_quintile_uf2018)

# 4.3 Sex --------------------------------------------------------------------------------------------------------------------------

frq(db_long$m0_sexo)

db_long <- db_long %>%
  mutate(
    # Recode sex and attach variable label
    sex = case_when(
      m0_sexo == 1 ~ "Male",
      m0_sexo == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("Male", "Female"))
  )

frq(db_long$sex)

# 4.4 Age --------------------------------------------------------------------------------------------------------------------------

frq(db_long$m0_edad)

db_long <- db_long %>%
  mutate(
    # Ensure age is numeric
    age = as.numeric(m0_edad),
    
    # Create age groups (categorical)
    age_t = case_when(
      age >= 18 & age <= 29  ~ "18-29",
      age >= 30 & age <= 49  ~ "30-49",
      age >= 50 & age <= 64  ~ "50-64",
      age >= 65 & age <= 150 ~ "65 or more",
      TRUE ~ NA_character_
    ),
    age_t = factor(age_t, levels = c("18-29", "30-49", "50-64", "65 or more")),
    
  )

frq(db_long$age)
frq(db_long$age_t)


# 4.5 Education -----------------------------------------------------------

frq(db_long$m01)

db_long <- db_long %>%
  mutate(
    # CINE-style categories
    cine = case_when(
      m01 %in% c(1, 2, 3)     ~ "Primary or less",
      m01 %in% c(4, 5)        ~ "Secondary",
      m01 %in% c(6, 7)        ~ "Technical",
      m01 %in% c(8, 9, 10)    ~ "University or more",
      TRUE ~ NA_character_
    ),
    cine = factor(
      cine,
      levels = c("Primary or less", "Secondary", "Technical", "University or more")
    ),
    
    # Dichotomized education
    educ_dic = case_when(
      m01 %in% c(1,2,3,4,5,6,7) ~ "Less than Universitary",
      m01 %in% c(8,9,10)        ~ "Universitary",
      m01 %in% c(-888, -999)    ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Education in years (based on CASEN 2017 mapping)
    educyear = case_when(
      m01 == 1              ~ 0,
      m01 == 2              ~ 4.3,
      m01 == 3              ~ 7.5,
      m01 == 4              ~ 9.8,
      m01 == 5              ~ 12.02,
      m01 == 6              ~ 13.9,
      m01 == 7              ~ 14.8,
      m01 == 8              ~ 14.9,
      m01 == 9              ~ 16.9,
      m01 == 10             ~ 19.07,
      m01 %in% c(-888, -999)~ NA_real_,
      TRUE                  ~ NA_real_
    )
  )

frq(db_long$cine)
frq(db_long$educ_dic)
frq(db_long$educyear)

# 4.6 create Household income ------------------------------------------------------------

# Impute midpoint of income ranges
db_long$m30_rec <-
  as.numeric(car::recode(
    db_long$m30,
    "1=110000;2=251000;3=305000;4=355000;5=400000;
     6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
     13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
     19=2275000;20=2700000;NA=NA;c(-888,-999)=NA"
  ))

# Impute midpoint of income ranges (2021)
db_long$m30b_rec <-
  as.numeric(car::recode(
    db_long$m30b,
    "1=125000;2=300000;3=400000;4=575000;5=700000;NA=NA;c(-888,-999)=NA"
  ))

sjmisc::frq(db_long$m30_rec)
sjmisc::frq(db_long$m30b_rec)

# Recode DK/DA of income to NA
db_long$m29_rec <-
  as.numeric(car::recode(db_long$m29, "c(-888,-999)=NA"))

# Replace missing continuous income with imputed midpoints
db_long$m29_imp <- ifelse(!is.na(db_long$m29_rec), db_long$m29_rec, db_long$m30_rec)
summary(db_long$m29_imp)

db_long$m29_imp <- ifelse(is.na(db_long$m29_imp), db_long$m30b_rec, db_long$m29_imp)
summary(db_long$m29_imp)

# Deflate to constant prices --------------------------------------------------

url <- "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007?cbFechaInicio=2016&cbFechaTermino=2025&cbFrecuencia=MONTHLY&cbCalculo=NONE&cbFechaBase="

ipc <- url %>%
  read_html() %>%
  html_node("table") %>%
  html_table() %>%
  rename_with(., ~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>%
  filter(serie == "Índice IPC General") %>%
  mutate(
    across(
      .cols = c(everything(), -serie),
      .fns = ~ as.numeric(str_replace(., ",", "."))
    )
  ) %>%
  select(-sel_) %>%
  pivot_longer(
    cols = -serie,
    names_to = "ano_mes",
    values_to = "ipc"
  ) %>%
  tidyr::separate(col = "ano_mes", into = c("mes", "ano"))

ipc <- ipc %>%
  filter(mes == "dic") %>%
  select(ano, ipc) %>% 
  slice_head(n = 4)

db_long$ola <- as.factor(db_long$ola)

db_long <- left_join(db_long, ipc, by = c("ola" = "ano"))

frq(db_long$ipc)

# Build household size -----------------------------------

db_long <- db_long %>% 
  mutate(ola = as.integer(ola))

db_long <- db_long %>%
  mutate(
    n_hogar = dplyr::case_when(
      ola == 2016 ~ nhogar1,
      ola == 2017 ~ m46_nhogar,
      ola == 2018 ~ m54,
      ola == 2019 ~ m54,
      TRUE ~ NA_real_
    )
  )

sjmisc::frq(db_long$n_hogar)

# Recode DK/DA of household size to NA
db_long$n_hogar_r <- car::recode(db_long$n_hogar, "c(-888,-999)=NA")
sjmisc::frq(db_long$n_hogar_r)

# Equivalised income (square-root scale) --------------------------------------

# Select a reference IPC (e.g., December 2023 as base)
ipc_ref <- db_long %>%
  filter(ola == 2019) %>%
  summarise(ipc_ref = median(ipc, na.rm = TRUE)) %>%
  pull(ipc_ref)

db_long <- db_long %>%
  mutate(
    # Deflate household income to constant prices (base = 2023 Dec)
    inc_hh_real = m29_imp * (ipc_ref / ipc),
    
    # Equivalence scale: square-root of household size
    eq_size_sqrt = if_else(!is.na(n_hogar_r) & n_hogar_r > 0,
                           sqrt(as.numeric(n_hogar_r)),
                           NA_real_),
    
    # Equivalised household income
    inc_eq_real_sqrt = inc_hh_real / eq_size_sqrt,
    
    # Log equivalised income (for modelling)
    ln_inc_eq_real_sqrt = if_else(!is.na(inc_eq_real_sqrt) & inc_eq_real_sqrt > 0,
                                  log(inc_eq_real_sqrt),
                                  NA_real_)
  )

sjmisc::descr(db_long$m29_imp)
sjmisc::descr(db_long$inc_eq_real_sqrt)
sjmisc::descr(db_long$ln_inc_eq_real_sqrt)

# Original per-capita measure for comparison -------------
db_long$ing_pc <- db_long$m29_imp / db_long$n_hogar_r
sjmisc::descr(db_long$ing_pc)

# Income deciles (within-wave, based on equivalised income) -------------------

db_long <- db_long %>%
  group_by(ola) %>%
  mutate(decile_eq = ntile(inc_eq_real_sqrt, 10)) %>%
  ungroup()

frq(db_long$decile_eq)

# Include missing cases for income groups
db_long$decile_eq1 <- if_else(is.na(db_long$decile_eq), "DNA", as.character(db_long$decile_eq))
db_long$decile_eq1 <- factor(db_long$decile_eq1, levels = c(as.character(1:10), "DNA"))

sjmisc::frq(db_long$decile_eq1)

frq(db_long$ola)  # ok

db_long <- db_long %>% 
  mutate(
    across(
      .cols = c(decile_eq, decile_eq1),
      .fns = ~ as.factor(.)
    )
  )


# 4.7 Create OESCH Class scheme -------------------------------------------

# ISCO 08

db_long <- db_long %>%
  rename(isco08 = ciuo08_m03) %>%
  mutate(
    isco08 = case_when(
      idencuesta == "13131014" ~ 2151,
      idencuesta == "13201011" ~ 3313,
      idencuesta == "13401034" ~ 7233,
      idencuesta == "13116018" ~ 8332,
      idencuesta == "13110111" ~ 5221,
      TRUE ~ isco08
    ),
    isco08 = case_when(
      isco08 == 15000 ~ 17, # retired
      isco08 == 16000 ~ 18, # unemployed
      TRUE ~ isco08
      ),
    isco08 = as.character(isco08))

db_long <- db_long %>% 
  mutate(
    isco08 = repair_isco(isco08))

# For household sostainer
db_long <- db_long %>%
  rename(isco08_sost = ciuo08_m22) %>% 
  mutate(isco08_sost = case_when(
    isco08_sost == 15000 ~ 17, # retired
    isco08_sost == 16000 ~ 18, # unemployed
    TRUE ~ isco08_sost),
    isco08_sost = as.character(isco08_sost))

frq(db_long$isco08)
frq(db_long$isco08_sost)

db_long <- db_long %>% 
  mutate(
    across(
      .cols = c(isco08, isco08_sost),
      .fns = ~ set_na(., na = c(9999))
    )
  )

#  SELF EMPLOYMENT -- EMPLOYMENT RELATION

frq(db_long$m07)

db_long <- db_long %>% 
  rowwise() %>% 
  mutate(self_employed = case_when(m07 %in% c(4:5) ~ 1,
                                   m07 %in% c(1,2,7) ~ 0,
                                   TRUE ~ NA)) %>% 
  ungroup()

frq(db_long$self_employed)

# SUPERVISION

frq(db_long$m06)

db_long$is_supervisor <- if_else(db_long$m06 >= 1, 1, 0)

frq(db_long$is_supervisor)

# N EMPLOYEES

frq(db_long$m06)

db_long$n_employees <- if_else(db_long$is_supervisor == 0, 0, db_long$m06) 

frq(db_long$n_employees)

## Create OESCH CLASS SCHEME

db_long <- db_long %>% 
    mutate(oesch5 = isco08_to_oesch(isco08, self_employed, n_employees, n_classes = 5, label = F),
           oesch5_sost = isco08_to_oesch(isco08_sost, self_employed, n_employees, n_classes = 5, label = F))

frq(db_long$oesch5)

frq(db_long$m02)

#Categorización de casos NA según situación laboral
db_long <- db_long %>% 
  mutate(
    situacion_na_oesch = case_when(
      !is.na(oesch5) ~ "Tiene OESCH",
      is.na(oesch5) & m02 == 3 ~ "Estudia y trabaja",
      is.na(oesch5) & m02 == 4 ~ "Solo estudia",
      is.na(oesch5) & m02 == 5 ~ "Jubilado o pensionado",
      is.na(oesch5) & m02 == 6 ~ "Desempleado, buscando trabajo",
      is.na(oesch5) & m02 == 7 ~ "Realiza tareas no remuneradas",
      is.na(oesch5) & m02 == 8 ~ "Enfermo o con discapacidad",
      is.na(oesch5) & m02 == 9 ~ "No estudia, no trabaja, no busca trabajo",
      is.na(oesch5) & m02 %in% c(1, 2) ~ "Ocupado sin OESCH",
      is.na(oesch5) & is.na(m02) ~ "Sin respuesta",
      TRUE ~ "Otro"
    )
  )

# Tabla cruzada por ola
library(sjPlot)
sjt.xtab(db_long$situacion_na_oesch, db_long$ola,
         show.col.prc = TRUE,
         var.labels = c("Situación laboral (casos NA OESCH)", "Ola"),
         show.summary = FALSE,
         title = NULL)


db_long <- db_long %>% 
  mutate(
    oesch5 = case_when(
      # Mantener categorías OESCH existentes
      !is.na(oesch5) ~ as.numeric(oesch5),
      # Categoría residual para quienes están fuera de la fuerza de trabajo
      is.na(oesch5) & m02 == 5 ~ 6,
      is.na(oesch5) & m02 == 6 ~ 7,
      is.na(oesch5) & m02 %in% c(3,4,7,8,9) ~ 8,
      # NA para casos sin información
      TRUE ~ NA_real_
    )) %>% 
  mutate(
    oesch5_sost = case_when(
      # Mantener categorías OESCH existentes
      !is.na(oesch5_sost) ~ as.numeric(oesch5_sost),
      # Categoría residual para quienes están fuera de la fuerza de trabajo
      is.na(oesch5_sost) & m21 == 5 ~ 6,
      is.na(oesch5_sost) & m21 == 6 ~ 7,
      is.na(oesch5_sost) & m21 %in% c(3,4,7,8,9) ~ 8,
      # NA para casos sin información
      TRUE ~ NA_real_
    ))

frq(db_long$oesch5)
frq(db_long$oesch5_sost)

db_long <- db_long %>% 
  rowwise() %>% 
  mutate(class = if_else(is.na(oesch5), oesch5_sost, oesch5)) %>% 
  ungroup()

# Label class variables 

db_long <- db_long %>%
    mutate(
      # Set to factor for the five categories version of class
      across(c(oesch5, oesch5_sost, class), ~ case_when(. == 1 ~ "Higher-grade service class",
                                                        . == 2 ~ "Lower-grade service class",
                                                        . == 3 ~ "Small business owners",
                                                        . == 4 ~ "Skilled workers",
                                                        . == 5 ~ "Unskilled workers",
                                                        . == 6 ~ "Retired",
                                                        . == 7 ~ "Unemployed",
                                                        . == 8 ~ "Other (NILF)",
                                                        TRUE ~ NA_character_))) %>% 
  mutate(
    across(c(oesch5, oesch5_sost, class), ~ factor(.,
                                                   levels = c(
                                                     "Higher-grade service class",
                                                     "Lower-grade service class",
                                                     "Small business owners",
                                                     "Skilled workers",
                                                     "Unskilled workers",
                                                     "Retired",
                                                     "Unemployed",
                                                     "Other (NILF)"))))
  

frq(db_long$class)

# 4.8 ISEI --------------------------------------------------------------

frq(db_long$isco08)
frq(db_long$isco08_sost)

db_long <- db_long %>% 
  mutate(isei = DIGCLASS::isco08_to_isei(isco08),
         isei_sost = DIGCLASS::isco08_to_isei(isco08_sost))

frq(db_long$isei)
frq(db_long$isei_sost)

db_long$isei <- as.numeric(db_long$isei)
db_long$isei_sost <- as.numeric(db_long$isei_sost)

db_long <- db_long %>% 
  rowwise() %>% 
  mutate(isei = if_else(is.na(isei), isei_sost, isei)) %>% 
  ungroup()

# 4.9 Winsorisation of income -------------------------------------------------------

db_long %>%
  group_by(ola) %>%
  summarise(
    n = sum(!is.na(inc_eq_real_sqrt)),
    mean = mean(inc_eq_real_sqrt, na.rm = TRUE),
    median = median(inc_eq_real_sqrt, na.rm = TRUE),
    p95 = quantile(inc_eq_real_sqrt, .95, na.rm = TRUE),
    p995 = quantile(inc_eq_real_sqrt, .99, na.rm = TRUE),
    p999 = quantile(inc_eq_real_sqrt, .999, na.rm = TRUE),
    max = max(inc_eq_real_sqrt, na.rm = TRUE)
  ) %>%
  arrange(ola) 

db_long %>%
  dplyr::filter(ola == 2016, !is.na(inc_eq_real_sqrt)) %>%
  arrange(desc(inc_eq_real_sqrt)) %>%
  select(idencuesta, ola, inc_eq_real_sqrt) %>%
  slice_head(n = 30)

tmp_2016 <- db_long %>% filter(ola == 2016, !is.na(inc_eq_real_sqrt))

p995_2016  <- quantile(tmp_2016$inc_eq_real_sqrt, .995,  na.rm = TRUE)

tmp_2016 %>% summarise(
  n = n(),
  n_gt_p995  = sum(inc_eq_real_sqrt > p995_2016))

# in 2016 3 cases above 995 percentile, 2 cases with values typed incorrectly

cap_2016 <- db_long %>%
  filter(ola == 2016, !is.na(inc_eq_real_sqrt)) %>%
  summarise(cap = sort(inc_eq_real_sqrt, decreasing = TRUE)[3]) %>%
  pull(cap)

db_long <- db_long %>%
  mutate(
    inc_eq_real_sqrt = if_else(
      ola == 2016 & !is.na(inc_eq_real_sqrt) & inc_eq_real_sqrt > cap_2016,
      cap_2016,
      inc_eq_real_sqrt
    )
  )

db_long %>%
  group_by(ola) %>%
  summarise(
    n = sum(!is.na(inc_eq_real_sqrt)),
    mean = mean(inc_eq_real_sqrt, na.rm = TRUE),
    median = median(inc_eq_real_sqrt, na.rm = TRUE),
    p95 = quantile(inc_eq_real_sqrt, .95, na.rm = TRUE),
    p995 = quantile(inc_eq_real_sqrt, .995, na.rm = TRUE),
    p999 = quantile(inc_eq_real_sqrt, .999, na.rm = TRUE),
    max = max(inc_eq_real_sqrt, na.rm = TRUE)
  ) %>%
  arrange(ola) 

# 4.11 Labels  -------------------------------------------------------------

db_long$idencuesta <- sjlabelled::set_label(db_long$idencuesta, label = "ID subject")
db_long$ola <- sjlabelled::set_label(db_long$ola, label = "Wave")
db_long$segmento <- sjlabelled::set_label(db_long$segmento, label = "Segment")
db_long$estrato <- sjlabelled::set_label(db_long$estrato, label = "Stratum")
db_long$ponderador_long_total <- sjlabelled::set_label(db_long$ponderador_long_total, label = "Longitudinal weight")
db_long$comuna <- sjlabelled::set_label(db_long$comuna, label = "Comune")
db_long$comuna_cod <- sjlabelled::set_label(db_long$comuna_cod, label = "Comune code")
db_long$region_cod <- sjlabelled::set_label(db_long$region_cod, label = "Region code")
db_long$geocodigo <- sjlabelled::set_label(db_long$geocodigo, label = "Geocode")
db_long$manzana_elsoc <- sjlabelled::set_label(db_long$manzana_elsoc, label = "ELSOC Manzana")

db_long$m33_rec <- sjlabelled::set_label(db_long$m33_rec, label = "Original housing recode")
db_long$housing <- sjlabelled::set_label(db_long$housing, label = "Housing ownership")
db_long$uf2018 <- sjlabelled::set_label(db_long$uf2018, label = "Price per m2 of land")
db_long$ln_uf2018 <- sjlabelled::set_label(db_long$ln_uf2018, label = "Log price per m2 of land")
db_long$decile_uf2018 <- sjlabelled::set_label(db_long$decile_uf2018, label = "Decile price per m2 of land")
db_long$quintile_uf2018 <- sjlabelled::set_label(db_long$quintile_uf2018, label = "Quintile price per m2 of land")
db_long$dummy_decile_uf2018 <- sjlabelled::set_label(db_long$dummy_decile_uf2018, label = "Decile 10 in land price")
db_long$dummy_quintile_uf2018 <- sjlabelled::set_label(db_long$dummy_quintile_uf2018, label = "Quintile 5 in land price")
db_long$area_sd_uf <- sjlabelled::set_label(db_long$area_sd_uf, label = "SD of price per m2 of land")

db_long$identification <- sjlabelled::set_label(db_long$identification, label = "Cultural identification")
db_long$friends <- sjlabelled::set_label(db_long$friends, label = "Number of friends")
db_long$size_network <- sjlabelled::set_label(db_long$size_network, label = "Nearby network size")
db_long$size_network_rec <- sjlabelled::set_label(db_long$size_network_rec, label = "Nearby network size dichotomized")
db_long$gen_trust <- sjlabelled::set_label(db_long$gen_trust, label = "Generalized trust in fellow citizens")
db_long$trust_minorities <- sjlabelled::set_label(db_long$trust_minorities, label = "Generalized trust in minorities")
db_long$trust_inst <- sjlabelled::set_label(db_long$trust_inst, label = "Trust in major institutions")
db_long$interest_pol <- sjlabelled::set_label(db_long$interest_pol, label = "Political engagement")
db_long$satisf_demo <- sjlabelled::set_label(db_long$satisf_demo, label = "Satisfaction with democracy")
db_long$conv_particip <- sjlabelled::set_label(db_long$conv_particip, label = "Conventional political participation")
db_long$unconv_particip <- sjlabelled::set_label(db_long$unconv_particip, label = "Unconventional political participation")
db_long$egalitarianism <- sjlabelled::set_label(db_long$egalitarianism, label = "Egalitarianism")
db_long$altruistic <- sjlabelled::set_label(db_long$altruistic, label = "Altruistic dispositions")
db_long$prosoc_behave <- sjlabelled::set_label(db_long$prosoc_behave, label = "Prosocial behavior")
db_long$democracy_support <- sjlabelled::set_label(db_long$democracy_support, label = "Support for democracy")
db_long$justif_violence <- sjlabelled::set_label(db_long$justif_violence, label = "Justification of violence")

db_long$inc_eq_real_sqrt <- sjlabelled::set_label(db_long$inc_eq_real_sqrt, label = "Equivalised household income (square-root scale)")
db_long$ln_inc_eq_real_sqrt <- sjlabelled::set_label(db_long$ln_inc_eq_real_sqrt, label = "Log equivalised household income (square-root scale)")
db_long$ing_pc <- sjlabelled::set_label(db_long$ing_pc, label = "Per capita household income")
db_long$decile_eq <- sjlabelled::set_label(db_long$decile_eq, label = "Income decile (equivalised, square-root scale)")
db_long$decile_eq1 <- sjlabelled::set_label(db_long$decile_eq1, label = "Income decile (equivalised) with NA")
db_long$class <- sjlabelled::set_label(db_long$class, label = "Social class")
db_long$sex <- sjlabelled::set_label(db_long$sex, label = "Sex")
db_long$age <- sjlabelled::set_label(db_long$age, label = "Age (in years)")
db_long$age_t <- sjlabelled::set_label(db_long$age_t, label = "Age")
db_long$cine <- sjlabelled::set_label(db_long$cine, label = "Educational level (CINE)")
db_long$educ_dic <- sjlabelled::set_label(db_long$educ_dic, label = "Universitary education")
db_long$educyear <- sjlabelled::set_label(db_long$educyear, label = "Education in years")
db_long$isei <- sjlabelled::set_label(db_long$isei, label = "ISEI")

# 4.12 Drop variables ------------------------------------------------------------------------------------------------------------------------------------------
db_long <- db_long %>% 
  select(
    idencuesta, ola, segmento, estrato, ponderador_long_total, 
    comuna, comuna_cod, region_cod, geocodigo, manzana_elsoc,
    housing, 
    uf2018, ln_uf2018, decile_uf2018, dummy_decile_uf2018, quintile_uf2018, dummy_quintile_uf2018,
    identification:justif_violence,
    inc_eq_real_sqrt, ln_inc_eq_real_sqrt, decile_eq, decile_eq1,
    isei, sex, age, educyear)
