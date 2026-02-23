#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Drop missing values
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 5.1 Missing values ------------------------------------------------------

colSums(is.na(db_long))
n_miss(db_long)
prop_miss(db_long)*100 # 1.6% de NAs
miss_var_summary(db_long)
vis_miss(db_long) + theme(axis.text.x = element_text(angle=80))

df_study1 <- db_long %>% drop_na()

#* removed 886 rows 29%, 2,191 rows remaining

# 5.2 Save and export -----------------------------------------------------
save(df_study1, file = here("output/data/df_study1_V2.RData"))
save(db_long, file = here("output/data/db_long_V2.RData"))

rm(list = ls())
