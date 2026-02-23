#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Selecting, filtering and recoding variables
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# General NA's
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

elsoc_long_2016_2023 <- elsoc_long_2016_2023 %>% 
  mutate(
    across(everything(), ~ remove_value_labels(., c(-666, -777, -888, -999))) # ! DROPS LABELS FROM SPECIAL VALUES
  )

# 1.1 Select and filter -----------------------------------------------------------

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola, 
         muestra,
         segmento,
         estrato,
         ponderador_long_total,
         ponderador01,
         ponderador02,
         comuna, 
         comuna_cod,
         region_cod,
         m33,
         c32_01, c32_02, # 1. Sense of belonging and identification
         r15, # 2. Number of friends
         r13_nredes, # 3. Intimate network size
         c02, # 4. Generalized trust
         c06_04, c06_05, c06_06, # 5. Trust in social minorities
         c05_01, c05_02, c05_05, c05_07, # 6. Trust in major institutions
         c13, # 7. Interest in political affairs
         c01, # 8. Satisfaction with democracy
         c12_01, c12_03, c12_04, c12_05, # 9. Conventional political participation
         c08_01, c08_02, c08_03, # 10. Unconventional political participation
         d02_01, d02_02, d02_03, # 11. Egalitarianism
         c18_02, c18_03, # 12. Altruistic disposition
         c07_04, c07_05, # 13. Pro-social behavior
         c25, # 14. Support for democracy
         f05_01, f05_02, f05_03, # 15. Justification of violence
         m0_sexo,
         m0_edad,
         m01,
         m02,
         ciuo88_m03,
         ciuo08_m03,
         ciuo88_m22,
         ciuo08_m22,
         m06, 
         m07,
         m19,
         m21,
         nhogar1,
         m46_nhogar,
         m54, 
         m30, 
         m30b, 
         m29) %>% 
  as_tibble() 


#  Filter to Santiago region -------------------------------------------

db <- db %>% 
  filter(region_cod == 13)

# filter: removed 5,197 rows (63%), 3,077 rows remaining

# 1.3 Recode -----------------------------------------------------------

# Wave
frq(db$ola)

db <- db %>%
  mutate(
    # Recode wave to survey year (and keep chronological order)
    ola = case_when(
      ola == 1 ~ 2016,
      ola == 2 ~ 2017,
      ola == 3 ~ 2018,
      ola == 4 ~ 2019,
      ola == 5 ~ 2021,
      ola == 6 ~ 2022,
      ola == 7 ~ 2023,
      TRUE ~ NA_real_
    ))

frq(db$ola)

# Specific dependent variables ------------------------------------------------------------

frq(db$c02)
frq(db$f05_01)
frq(db$f05_02)
frq(db$f05_03)
frq(db$r13_nredes)
frq(db$c25)

db <- db %>%
  # Generalized trust: recode in order to create an ordinal variable (+gen_trust -> +attachment to society)
  mutate(
    # Recoding values
    across(starts_with("c02"), ~ case_when(
      . == 1 ~ 3,
      . == 3 ~ 2,
      . == 2 ~ 1,
      TRUE ~ .
    )),
    # Recoding labels
    across(starts_with("c02"), ~ set_labels(.,
                                            labels = c(
                                              "Almost always, one must be careful when dealing with people" = 1,
                                              "It depends" = 2,
                                              "Almost always, one can trust people" = 3
                                            )
    ))
  ) %>%
  # Justification of violence: invert the scale in order to +justif_violence -> +attachment to society
  mutate(
    across(starts_with("f05"), ~ invert_scale(.)), # Recoding values
    # Recoding labels
    across(starts_with("f05"), ~ set_labels(.,
                                            labels = c(
                                              "It is always justified" = 1,
                                              "It is often justified" = 2,
                                              "It is sometimes justified" = 3,
                                              "It is rarely justified" = 4,
                                              "It is never justified" = 5
                                            )
    ))
  ) %>%
  # Size network: create binary variable
  mutate(
    across(starts_with("r13"), ~ if_else(. >= median(., na.rm = T), 1, 0), .names = "rec_{.col}"), # Recoding values
    across(starts_with("_rec"), ~ set_labels(., labels = c("Below the median" = 0, "Equal to or above the median" = 1))) # Recoding labels
  ) %>%
  # Democracy support: recode the scale in order to +democracy support -> +attachment to society
  mutate(
    # Recoding values
    across(starts_with("c25"), ~ case_when(
      . == 2 ~ 1,
      . %in% c(3, 4) ~ 2,
      . == 1 ~ 3
    )),
    # Recoding labels
    across(starts_with("c25"), ~ set_labels(.,
                                            labels = c(
                                              "In some circumstances, an authoritarian government may be preferable to a democratic one" = 1,
                                              "People like us don't care whether the regime is democratic or authoritarian / None" = 2,
                                              "Democracy is preferable to any other form of government" = 3
                                            )
    ))
  ) %>%
  # Egalitarianism: invert the scale in order to +egalitarianism -> +attachment to society
  mutate(
    across(starts_with("d02"), ~ invert_scale(.)), # Recoding values
    # Recoding labels
    across(starts_with("d02"), ~ set_labels(.,
                                            labels = c(
                                              "Strongly agree" = 1,
                                              "Agree" = 2,
                                              "Neither agree nor disagree" = 3,
                                              "Disagree" = 4,
                                              "Strongly disagree" = 5
                                            )
    ))
  )


# Remove from the global enviroment
rm(remove_value_labels, invert_scale)
