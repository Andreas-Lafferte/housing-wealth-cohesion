

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
