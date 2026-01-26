#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Processing code for a research paper on "Housing wealth and social cohesion: Evidence from Chile"
# Responsable: Technical assistant
# Executive Summary: This script contains the code to run all parts of the data processing process.
# Date: December 10, 2025
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               haven,
               tidylog,
               rlang,
               sjlabelled,
               RStata,
               ISCO08ConveRsions,
               visdat,
               car,
               sjmisc, 
               here,
               rio,
               rvest,
               DIGCLASS,
               naniar)

# 2. Data -----------------------------------------------------------------
load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

geo_elsoc <- rio::import(here("input/data/geo_elsoc.dta"))

precios_suelo <- rio::import(here("input/data/precios_suelo.dta"))

insumo_ciuo <- readxl::read_xlsx("input/insumo_ciuo.xlsx") # Source document with the equivalences between ciuo88 and ciuo08 (ciuo = isco)

insumo_oesch <- readxl::read_excel("input/Final_proposition_passage_ISCO08_Oesch_10_06_2014.xls") %>% # Source document with equivalences between isco and oesch class scheme
  select("isco" = 1, "description" = 2, "class" = 3) %>%
  mutate(isco = as.numeric(isco)) %>%
  select(-description) %>%
  mutate(class = as.numeric(if_else(class == "leave aside", NA, class)))

source("processing/functions.R", encoding = "UTF-8") # Utility functions for processing data

# 3. Run scripts  ---------------------------------------------------------------

source("processing/1_recode.R", encoding = "UTF-8") 
source("processing/2_impute.R", encoding = "UTF-8") 
source("processing/3_merge.R", encoding = "UTF-8") 
source("processing/4_create_variables.R", encoding = "UTF-8") 
source("processing/5_drop_na.R", encoding = "UTF-8") 

# 4. Save data --------------------------------------------------------------------------------------------------------------------------------------------
save.image("output/data/elsoc_proc.RData") # Saving whole workspace

