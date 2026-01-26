#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Filter and drop missing values
# Responsable: Technical assistant
#******************************************************************************************************************************************************


# 5.1 Filter to Santiago region -------------------------------------------

db_long <- db_long %>% 
  filter(region_cod == 13)

# 5.2 Missing values ------------------------------------------------------

colSums(is.na(db_long))
n_miss(db_long)
prop_miss(db_long)*100 # 1.3% de NAs
miss_var_summary(db_long)
vis_miss(db_long) + theme(axis.text.x = element_text(angle=80))

df_study1 <- db_long %>% drop_na()

#* removed 684 rows -22%-, 2,393 rows remaining

# 5.3 Winsorisation -------------------------------------------------------

df_study1 %>%
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

df_study1 %>%
  dplyr::filter(ola == 2016, !is.na(inc_eq_real_sqrt)) %>%
  arrange(desc(inc_eq_real_sqrt)) %>%
  select(idencuesta, ola, inc_eq_real_sqrt) %>%
  slice_head(n = 30)

tmp_2016 <- df_study1 %>% filter(ola == 2016, !is.na(inc_eq_real_sqrt))

p995_2016  <- quantile(tmp_2016$inc_eq_real_sqrt, .995,  na.rm = TRUE)

tmp_2016 %>% summarise(
  n = n(),
  n_gt_p995  = sum(inc_eq_real_sqrt > p995_2016))

# in 2016 3 cases above 995 percentile, 2 cases with values typed incorrectly

cap_2016 <- df_study1 %>%
  filter(ola == 2016, !is.na(inc_eq_real_sqrt)) %>%
  summarise(cap = sort(inc_eq_real_sqrt, decreasing = TRUE)[3]) %>%
  pull(cap)

df_study1 <- df_study1 %>%
  mutate(
    inc_eq_real_sqrt = if_else(
      ola == 2016 & !is.na(inc_eq_real_sqrt) & inc_eq_real_sqrt > cap_2016,
      cap_2016,
      inc_eq_real_sqrt
    )
  )

df_study1 %>%
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


# 5.4 Clusters CASEN ------------------------------------------------------

frq(df_study1$comuna)

# no hay comunas del cluster 2; 
# solo hay 2 comunas del cluster 3;
# se pierden comunas en el cluster 4

df_study1 <- df_study1 %>% 
  mutate(
    cluster_c = case_when(
      comuna %in% c("Pudahuel",
                    "El bosque", 
                    "Conchali",
                    "Lo espejo",
                    "San joaquin",
                    "Cerro navia",
                    "La pintana",
                    "Recoleta",
                    "San ramon",
                    "Pedro aguirre cerda",
                    "La granja",
                    "San bernardo",
                    "Estacion central") ~ "1",
      comuna %in% c("Padre hurtado",
                    "Cerrillos") ~ "2",
      comuna %in% c("Maipu",
                    "La florida",
                    "La cisterna",
                    "Pennalolen",
                    "Renca",
                    "Puente alto",
                    "Quinta normal",
                    "Quilicura",
                    "Lo prado",
                    "Independencia",
                    "Huechuraba") ~ "3",
      comuna %in% c("Nnunnoa",
                    "La reina",
                    "San miguel",
                    "Santiago",
                    "Macul") ~ "4",
      comuna %in% c("Vitacura",
                    "Las condes",
                    "Providencia",
                    "Lo barnechea") ~ "5",
      TRUE ~ NA_character_
      
    )
  )

frq(df_study1$cluster_c)

# 5.5 Save and export -----------------------------------------------------
save(df_study1, file = here("output/data/df_study1_V2.RData"))
save(db_long, file = here("output/data/db_long_V2.RData"))

rm(list = ls())
