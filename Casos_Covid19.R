library(tidyverse)
library(dplyr)
library(lubridate)

getwd()
setwd('~/Desktop/Prueba Técnica')

# Importamos la Data
C19 <- read_csv('~/Desktop/Prueba Técnica/Covid.csv')

# Transformamos la data para que se vean las fechas en formato
# largo. 

covid19 <- C19 %>%
  pivot_longer(-c(Region),
      names_to = "Fecha",
      values_to = "Confirmados") %>%
  group_by(Region, Fecha) %>%
  summarise(Confirmados = sum(Confirmados)) %>%
  ungroup()

# class(covid19$Fecha)

covid19$Fecha <- as.Date(covid19$Fecha)

# class(covid19$Fecha)

# Primer caso de covid en Chile

covid <- covid19 %>% 
  arrange(Fecha) %>% 
  group_by(Fecha, Confirmados) %>%
  filter(Fecha == min(Fecha)) %>%
  filter(Region != 'Total' & Confirmados != 0) %>%
  slice(1) %>%
  ungroup()

# Que día superaron los 1000 casos

covid %>% 
  filter(Region == 'Araucanía' & Confirmados >= 1000)

# Que región reportó el mayor número de casos en x fecha

covid %>%
  filter(Fecha == '2020-04-17') %>%
  arrange(desc(Confirmados))

# Ranking de las Regiones con más contagios

covid %>%
  group_by(Region) %>%
  filter(Fecha >= '2020-03-03' & Fecha <= '2020-06-01') %>%
  summarise(Confirmados = sum(Confirmados)) %>%
  arrange(desc(Confirmados))

# Casos Nuevos

covid2 <- covid %>%
  arrange(Fecha) %>%
  group_by(Region) %>%
  mutate(Nuevos_Casos = Confirmados - lag(Confirmados, default = 0)) %>%
  ungroup()

# Plot - 1

covid2 %>% 
  filter(Region %in% c('O’Higgins', 'Los Lagos')) %>%
  ggplot(aes(x = Fecha, y = Nuevos_Casos, color = Region)) + 
  geom_line() + 
  scale_x_date(date_breaks = '1 month', date_labels = "%b %y") + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~ Region, ncol = 1, scales = "free_y") +
  labs(x = "Fecha", y = "Casos Nuevos", 
       title = "Casos Confirmados de Covid-19")
