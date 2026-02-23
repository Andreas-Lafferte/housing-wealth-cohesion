

df_study1 %>% 
  count(comuna, dummy_decile_uf2018, name = "n") %>% 
  group_by(comuna) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(all(c(0, 1) %in% dummy_decile_uf2018))


df_study1 %>% 
  count(comuna, dummy_decile_uf2018, name = "n") %>% 
  group_by(comuna) %>% 
  mutate(prop = n / sum(n)) %>% 
  print(n = 40)

df_study1 %>% 
  count(comuna, dummy_decile_uf2018, name = "n") %>% 
  group_by(comuna) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(dummy_decile_uf2018 == 1)

df_study1 %>%
  group_by(idencuesta, comuna) %>% 
  summarise(
    dummy_any1 = as.integer(any(dummy_decile_uf2018 == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  count(comuna, dummy_any1, name = "n") %>%
  group_by(comuna) %>%
  mutate(prop = n / sum(n)) %>%
  filter(all(c(0, 1) %in% dummy_any1))


df_study1 %>%
  arrange(idencuesta, ola) %>% 
  group_by(idencuesta) %>%
  summarise(
    comuna = dplyr::last(comuna),
    dummy_any1 = as.integer(any(dummy_decile_uf2018 == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  count(comuna, dummy_any1, name = "n") %>%
  group_by(comuna) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>% 
  filter(dummy_any1 == 1)

# puedes graficar la distribución de precio de suelo por percentil de precio de suelo?

df_study1 %>% 
  select(idencuesta, comuna, uf2018, ln_uf2018, decile_uf2018, dummy_decile_uf2018) %>% 
  group_by(decile_uf2018) %>% 
  summarise(
    mean = mean(uf2018),
    sd = sd(uf2018)
  )
  
  
pctl <- df_study1 %>%
  summarise(
    p = 1:100,
    uf2018_p = as.numeric(quantile(uf2018, probs = p/100, na.rm = TRUE))
    # alternativa en log:
    # ln_uf2018_p = as.numeric(quantile(ln_uf2018, probs = p/100, na.rm = TRUE))
  )

ggplot(pctl, aes(x = p, y = uf2018_p)) +
  geom_line() +
  labs(x = "Percentil de precio de suelo (uf2018)", y = "Precio de suelo (UF/m²)")


pctl_ln <- df_study1 %>%
  summarise(
    p = 1:100,
    ln_uf2018_p = as.numeric(quantile(ln_uf2018, probs = p/100, na.rm = TRUE))
  )

ggplot(pctl_ln, aes(x = p, y = ln_uf2018_p)) +
  geom_line() +
  labs(x = "Percentil de precio de suelo (ln_uf2018)", y = "ln(precio de suelo)")


df_study1 %>%
  filter(!is.na(decile_uf2018), !is.na(uf2018)) %>%
  mutate(decile_uf2018 = factor(decile_uf2018, levels = 1:10)) %>%
  ggplot(aes(x = decile_uf2018, y = uf2018)) +
  geom_boxplot() +
  labs(x = "Decil de precio de suelo", y = "Precio de suelo (UF/m²)")

df_study1 %>%
  filter(!is.na(uf2018)) %>%
  mutate(pctl_uf2018 = ntile(uf2018, 100)) %>%
  ggplot(aes(x = factor(pctl_uf2018), y = uf2018)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(x = "Percentil (bin 1–100) de precio de suelo", y = "Precio de suelo (UF/m²)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


frq(df_study1$decile_uf2018)
frq(df_study1$quintile_uf2018)


df_study1 %>% 
  transmute(q = ntile(uf2018, 10)) %>% 
  frq()
