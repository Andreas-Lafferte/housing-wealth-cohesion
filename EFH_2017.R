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
               sjlabelled,
               RStata,
               car,
               sjmisc, 
               here,
               rio)

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

efh <- rio::import(here("input/data/EFH/Base no imputada EFH 2017.dta"))

names(efh)
glimpse(efh)

find_var(efh, "zona")
