#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Separate elsocs per year and merge geo vars
# Responsable: Technical assistant
#******************************************************************************************************************************************************

# 3.1 Drop variables --------------------------------------------------------------------------------------------------------------------------------

db_wide <- db_wide %>%
  # Drop useless variables
  select(-starts_with("ciuo88_m03"), 
         -starts_with("ciuo88_m22"), 
         -starts_with("m19"), 
         -starts_with("ponderador01"), 
         -starts_with("ponderador02")) %>% 
  # ------------------------------------------------------------
  # !Drop waves we dont use: IMPORTANT
  # ------------------------------------------------------------
  select(-matches("_w05$|_w06$|_w07$"))

# 3.2 Separate elsoc wide data into datasets per year -----------------------------------------------------------------------------------------------

# Create function for separating elsoc
separate_elsoc <- function(data, suffix_wave) {
  results <- data %>%
    select(idencuesta, muestra, ends_with(suffix_wave)) %>% # Select essential variables
    rename_with(~ (str_replace_all(., suffix_wave, ""))) %>% # Remove suffix of wave
    filter(estrato %in% c(1:4)) # ! IMPORTANT: FILTER SAMPLE BY MAIN CITIES
  
  print(NROW(results))
  return(results)
}

# Separate elsoc
elsocs <- list(
  elsoc_2016 = separate_elsoc(db_wide, "_w01"), #* 1,894 rows remaining
  elsoc_2017 = separate_elsoc(db_wide, "_w02"), #* 1,609 rows remaining
  elsoc_2018 = separate_elsoc(db_wide, "_w03"), #* 2,508 rows remaining
  elsoc_2019 = separate_elsoc(db_wide, "_w04") #* 2,263 rows remaining
)

rm(separate_elsoc)

# 3.3 Geo variables -------------------------------------------------------

precios_suelo <- precios_suelo %>% 
  select(comuna,
         geocodg,
         uf2018) %>% 
  mutate(comuna = as.character(comuna),
         geocodg = as.character(geocodg),
         decile_uf2018 = ntile(uf2018, 10),
         quintile_uf2018 = ntile(uf2018, 5))

geo_merge <- inner_join(geo_elsoc, 
                       precios_suelo,
                       by = c("cod_comuna" = "comuna", "geocodigo" = "geocodg"))

# matched rows 3,691; rows only that match in precios suelo 1,605 

geo_merge <- geo_merge %>% 
  drop_na() %>% 
  mutate(base = as.integer(base),
         idencuesta = as.numeric(idencuesta),
         cod_comuna = as.numeric(cod_comuna))

# 3.3 Join geo elsoc variables -------------------------------------------------------------------------------------------------------------------

elsocs <- imap(elsocs, function(df, nm) {
  
  # Extract year from the list element name (e.g., "elsoc_2016" -> 2016)
  year <- as.integer(str_extract(nm, "\\d{4}"))
  
  # Subset the geo data for that year
  geo_year <- geo_merge %>%
    filter(base == year) %>%
    select(-base)
  
  # Left join
  left_join(
    df,
    geo_year,
    by = c("idencuesta" = "idencuesta",
           "comuna_cod" = "cod_comuna")
  )
})


#* elsoc_2016: match 715 rows 
#* elsoc_2017: match 592 rows 
#* elsoc_2018: match 926 rows 
#* elsoc_2019: match 811 rows 

# Remove objects from global enviroment
rm(db, elsoc_long_2016_2023, geo_elsoc, precios_suelo)


# 3.4 Wide to long --------------------------------------------------------

db_long <- imap_dfr(elsocs, function(df, nm) {
  year <- as.integer(str_extract(nm, "\\d{4}"))
  df %>% mutate(ola = year) %>% 
    select(idencuesta, ola, everything())
})

rm(elsocs)
