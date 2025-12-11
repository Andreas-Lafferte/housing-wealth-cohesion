# 0. Identification ---------------------------------------------------

# Title: Data preparation for research project "Housing wealth and social cohesion: Evidence from Chile"
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code to data preparation for the paper
# Date: December 10, 2025

# 1. Packages  -----------------------------------------------------

if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               car,
               sjmisc, 
               here,
               sjlabelled,
               SciViews,
               naniar,
               haven,
               rio)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))
geo_elsoc <- rio::import(here("input/data/geo_elsoc.dta"))
precios_suelo <- rio::import(here("input/data/precios_suelo.dta"))

glimpse(elsoc_long_2016_2023)
glimpse(geo_elsoc)
glimpse(precios_suelo)

# 3. Processing -----------------------------------------------------------

# General NA's
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

# 3.1 Select -----------------------------------------------------------

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola, 
         tipo_atricion,
         muestra,
         segmento,
         estrato,
         ponderador_long_total,
         ponderador01,
         ponderador02,
         comuna, 
         comuna_cod,
         region,
         region_cod,
         m0_sexo,
         m0_edad,
         m01,
         nhogar1,
         m46_nhogar,
         m54, 
         m30, 
         m30b, 
         m29, 
         m33
         ) %>% 
  as_tibble() 

# 3.2 Filter -----------------------------------------------------------

db <- db %>% 
  filter(region_cod == 13) # keep only region metropolitana

# 3.3 Recode and transform -----------------------------------------------------------

# Ola
frq(db$ola)

db <- db %>% 
  mutate(ola = case_when(ola == 1 ~ "2016",
                         ola == 2 ~ "2017",
                         ola == 3 ~ "2018",
                         ola == 4 ~ "2019",
                         ola == 5 ~ "2021",
                         ola == 6 ~ "2022",
                         ola == 7 ~ "2023"),
         ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2021",
                                      "2022",
                                      "2023")))


# Atricion y tipo muestra
frq(db$tipo_atricion)
frq(db$muestra)

# sex
frq(db$m0_sexo)
db$sex <- car::recode(db$m0_sexo, 
                      recodes = c("1='Male'; 2='Female'"), 
                      levels = c("Male", "Female"),
                      as.factor = T)

db$sex <- sjlabelled::set_label(db$sex, label = "Sex")
frq(db$sex)

# age
frq(db$m0_edad)

db$age <- as.numeric(db$m0_edad)

db$age_t <- factor(car::recode(db$age, 
                     "18:29=1;30:49=2;50:64=3;65:150=4"),
         labels = c('18-29', '30-49', '50-64', '65 or more'))


db$age <- sjlabelled::set_label(db$age, 
                                  label = c("Age")) 

db$age_t <- sjlabelled::set_label(db$age_t, 
                        label = c("Age groups")) 
frq(db$age)
frq(db$age_t)

# education
frq(db$m01)

db <- db %>% 
  mutate(cine = case_when(m01 %in% c(1,2,3) ~ "Primary or less",
                          m01 %in% c(4,5) ~ "Secondary",
                          m01 %in% c(6,7) ~ "Technical",
                          m01 %in% c(8,9,10) ~ "University or more",
                          TRUE ~ NA_character_),
         cine = factor(cine, 
                       levels = c("Primary or less",
                                  "Secondary",
                                  "Technical",
                                  "University or more")))

db$cine <-
  sjlabelled::set_label(x = db$cine,
                        label = "Education (CINE)")

frq(db$cine)

db$educ_dic <- car::recode(db$m01,
                           "c(1,2,3,4,5,6,7)=1;c(8,9,10)=2; c(-888,-999)=NA")

db$educ_dic <- factor(db$educ_dic,
                      labels = c("Less than Universitary","Universitary"))

db$educ_dic <-  sjlabelled::set_label(x = db$educ_dic,
                                      label = "Education (dichotomized)")

frq(db$educ_dic)

#Recoding of education to years based on casen 2017.

db$educyear<- as.numeric(car::recode(db$m01, 
              "1=0;2=4.3;3=7.5;4=9.8;5=12.02;6=13.9;
               7=14.8;8=14.9;9=16.9;10=19.07;c(-888,-999)=NA", 
              as.numeric = T))

db$educyear <- 
  sjlabelled::set_label(x = db$educyear,
                        label = "Education in years")

frq(db$educyear)

# Income

# N Household:
# Select variables______________________________________________________________
# Household income_________________________________________

#Impute midpoint of income ranges
db$m30_rec <-
  as.numeric(car::recode(db$m30,
                         "1=110000;2=251000;3=305000;4=355000;5=400000;
            6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
            13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
            19=2275000;20=2700000;NA=NA;c(-888,-999)=NA"))

#Impute midpoint of income ranges (2021)
db$m30b_rec <-
  as.numeric(car::recode(db$m30b,
                         "1=125000;2=300000;3=400000;4=575000;5=700000;NA=NA;c(-888,-999)=NA"))

sjmisc::frq(db$m30_rec)
sjmisc::frq(db$m30b_rec)

#Recode DK/DA of Income to NA
db$m29_rec <-
  as.numeric(car::recode(db$m29,"c(-888,-999)=NA"))

#replace NA of income with new imputed variable
db$m29_imp <- 
  ifelse(test = !is.na(db$m29_rec),
         yes =  db$m29_rec,
         no =  db$m30_rec)
summary(db$m29_imp)

db$m29_imp <- 
  ifelse(test = is.na(db$m29_imp),
         yes =  db$m30b_rec,
         no =  db$m29_imp)
summary(db$m29_imp)

# deflate at each year's prices
library(rvest)

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
    )) %>% 
  select(-sel_) %>% 
  pivot_longer(., cols = -serie,
               names_to = "ano_mes",
               values_to = "ipc") %>% 
  tidyr::separate(col = "ano_mes", into = c("mes", "ano"))

ipc <- ipc %>% 
  filter(mes == "dic") %>% 
  select(ano, ipc)

db <- left_join(db, ipc, by = c("ola" = "ano"))

frq(db$ipc)

# Reshape long to wide
db_wide <- db %>% 
  tidyr::pivot_wider(id_cols = c("idencuesta","muestra"),
                     names_from = "ola",
                     values_from = names(select(db, tipo_atricion, segmento:ipc))
  )

db_wide$m54_2022 <- db_wide$m54_2023

# reshape from long to wide
db_long <- db_wide %>%
  pivot_longer(
    cols = -c(idencuesta, muestra),
    names_to = c(".value", "ola"),
    # Toma TODO lo que va antes del último "_" como nombre de variable,
    # y lo que va después como la ola (1..7)
    names_pattern = "^(.*)_(\\d+)$",
    values_drop_na = T
  ) %>%
  mutate(ola = as.integer(ola))

db_long <-
  db_long %>%
  mutate(n_hogar =
           dplyr::case_when(ola == 2016 ~ nhogar1,
                            ola == 2017 ~ m46_nhogar,
                            ola == 2018 ~ m54,
                            ola == 2019 ~ m54,
                            ola == 2021 ~ m54,
                            ola == 2022 ~ m54,
                            ola == 2023 ~ m54))
sjmisc::frq(db_long$n_hogar)

#Recode DK/DA to NA
db_long$n_hogar_r<-
  car::recode(db_long$n_hogar,"c(-888,-999)=NA")

# Per capita household income:
db_long$ing_pc <- 
  (db_long$m29_imp/db_long$n_hogar_r)

db_long$ing_pc <-
  sjlabelled::set_label(x = db_long$ing_pc,
                        label = "Per capita household income")  

sjmisc::descr(db_long$ing_pc)

# Compute income deciles: 
db_long <- db_long %>% 
  group_by(ola) %>% 
  mutate(
    decile = ntile(ing_pc, 10)) %>% 
  ungroup()

frq(db_long$decile)

db_long$decile <- 
  sjlabelled::set_label(x = db_long$decile,
                        label = "Income decile")  

# Include missing cases for income groups
db_long$decile1 <- if_else(is.na(db_long$decile), "DNA", as.character(db_long$decile))

db_long$decile1 <- factor(db_long$decile1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "DNA"))

db_long$decile1 <- 
  sjlabelled::set_label(x = db_long$decile1,
                        label = "Income decile with NA")  

sjmisc::frq(db_long$decile1)

frq(db_long$ola) #ok

# Housing

frq(db_long$m33)

db_long <- db_long %>% 
  mutate(
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
    m33_rec = factor(m33_rec,
                     levels = c("Owned and fully paid-off home",
                                "Owned home with mortgage payments",
                                "A rented home",
                                "A home provided by work or service",
                                "Owned home shared with one or more families",
                                "Home provided by a family member or friend",
                                "Other, please specify"))
  )


frq(db_long$m33_rec)

db_long <- db_long %>% 
  mutate(
    housing = case_when(
      m33 == 1 ~ "Owned and fully paid-off home",
      m33 == 2 ~ "Owned home with mortgage payments",
      m33 == 3 ~ "Rented housing",
      m33 %in% c(4,5,6) ~ "Other regime",
      TRUE ~ NA_character_
    ),
    housing = factor(housing, 
                     levels = c("Owned and fully paid-off home", 
                                "Owned home with mortgage payments",
                                "Rented housing", 
                                "Other regime")) 
  )

frq(db_long$housing)

## Descriptive for housing

library(shadowtext)
library(ggdist)

datos.housing <- db_long %>% 
  group_by(idencuesta, ola) %>% 
  count(housing) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2021",
                                      "2022",
                                      "2023")))


etiquetas.housing <- db_long %>%
  group_by(ola, housing) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(idencuesta = 1,
         ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2021",
                                      "2022",
                                      "2023")))


datos.housing %>% 
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
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2023 (N obs = 3,469; N individuals = 1,364)")+
       theme_ggdist() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) 

db_long %>% 
  select(idencuesta, ola, housing) %>% 
  drop_na() %>% 
  distinct(idencuesta)

datos.housing2 <- db_long %>% 
  filter(ola %in% c(2016, 2023)) %>% 
  group_by(idencuesta, ola) %>% 
  count(housing) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(ola = factor(ola, levels = c("2016",
                                      "2023")))


etiquetas.housing2 <- db_long %>%
  filter(ola %in% c(2016, 2023)) %>% 
  group_by(ola, housing) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(idencuesta = 1,
         ola = factor(ola, levels = c("2016",
                                      "2023")))


datos.housing2 %>% 
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
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2023 (N obs = 3,469; N individuals = 1,364)")+
  theme_ggdist() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) 

datos.m33 <- db_long %>% 
  group_by(idencuesta, ola) %>% 
  count(m33_rec) %>% 
  group_by(ola) %>% 
  mutate(porcentaje=n/sum(n)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2021",
                                      "2022",
                                      "2023")))


etiquetas.m33 <- db_long %>%
  group_by(ola, m33_rec) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ola) %>%
  mutate(porcentaje = count / sum(count)) %>% 
  na.omit() %>% 
  mutate(idencuesta = 1,
         ola = factor(ola, levels = c("2016",
                                      "2017",
                                      "2018",
                                      "2019",
                                      "2021",
                                      "2022",
                                      "2023")))


datos.m33 %>% 
  ggplot(aes(x = ola, fill = m33_rec, stratum = m33_rec,
             alluvium = idencuesta, y = porcentaje)) +
  ggalluvial::geom_flow(alpha = .4) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = viridis::viridis(7, option = "plasma")) +
  geom_shadowtext(data = etiquetas.m33,
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
       caption = "Source: own elaboration with pooled data from ELSOC 2016-2023 (N obs = 3,469; N individuals = 1,364)")+
  theme_ggdist() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) 

# Sex and housing

df <- subset(db_long, ola == 2016)

sjPlot::sjt.xtab(df$sex, 
                   df$housing, 
                   show.row.prc = T,
                   show.col.prc = T,
                   statistics = NULL)

# Age_t and housing

sjPlot::sjt.xtab(df$age_t, 
                 df$housing, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

# Decile and housing

sjPlot::sjt.xtab(df$decile, 
                 df$housing, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

# Education and housing

sjPlot::sjt.xtab(df$cine, 
                 df$housing, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

sjPlot::sjt.xtab(df$educ_dic, 
                 df$housing, 
                 show.row.prc = T,
                 show.col.prc = T,
                 statistics = NULL)

df %>% 
  select(housing, educyear) %>% 
  group_by(housing) %>% 
  summarise(educ = mean(educyear, na.rm = T))


# 3.4 Missing values ------------------------------------------------------
 # no aplica por ahora

# 4. Merge data sets -------------------------------------------------------

glimpse(db_long)
glimpse(geo_elsoc)
glimpse(precios_suelo)

precios_suelo <- precios_suelo %>% 
  select(comuna,
         geocodg,
         uf2018) %>% 
  mutate(comuna = as.character(comuna),
         geocodg = as.character(geocodg))

geo_merge <- left_join(geo_elsoc, 
          precios_suelo,
          by = c("cod_comuna" = "comuna", "geocodigo" = "geocodg"))

colSums(is.na(geo_merge))

geo_merge <- geo_merge %>% 
  drop_na() %>% 
  mutate(base = as.integer(base),
         idencuesta = as.numeric(idencuesta),
         cod_comuna = as.numeric(cod_comuna))

frq(geo_merge$base)

db_long_merge <- left_join(db_long, geo_merge, by = c("idencuesta" = "idencuesta", 
                                     "ola" = "base", 
                                     "comuna_cod" = "cod_comuna"))

# 5. Save and export ------------------------------------------------------

df_study1 <- db_long_merge %>% 
  filter(ola <= 2019) %>% 
  select(idencuesta, ola, segmento, estrato, ponderador_long_total, comuna,
         comuna_cod, sex, age, age_t, cine, educ_dic, educyear, decile, decile1,
         housing, geocodigo, manzana_elsoc, tipo_zona, uf2018) 

glimpse(df_study1)
