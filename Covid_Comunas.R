library(tidyverse)
library(dplyr)
library(lubridate)

theme_set(theme_minimal())

# .rs.restartR()

setwd("~/Desktop/R/Git/Covid19")
comunas_raw <- read_csv("~/Desktop/R/Git/Covid19/Covid-19.csv")

comunas_raw <- comunas_raw[,-2]
comunas_raw <- comunas_raw[,-3]
comunas_raw <- comunas_raw[,-3]
# comunas_raw <- comunas_raw[,-1]
comunas_raw <- comunas_raw[,-104]

complete.cases(comunas_raw)
comunas_raw <- na.omit(comunas_raw)
complete.cases(comunas_raw)

comunas_raw <- as_tibble(comunas_raw)
# write_csv(comunas_raw, 'comunas_raw.csv')

comunas <- comunas_raw %>% 
  pivot_longer(-c(Region, Comuna),
               names_to = "date",
               values_to = "confirmed_n") %>%
  rename(region = Region,
         comuna = Comuna) %>%
  group_by(region, comuna, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()

comunas$date <- as.Date(comunas$date)

comunas <- comunas %>% 
  arrange(date) %>%
  group_by(comuna) %>% 
  mutate(new_cases = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()

comunas %>%
  filter(comuna == "Paine", date >= "2021-01-01") %>%
  ggplot(aes(x = date, y = new_cases)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Fecha", y = "Casos Nuevos",
       title = "Casos confirmados de COVID-19") 
  
comunas %>%
  filter(comuna %in% c("Nunoa", "Santiago")) %>%
  ggplot(aes(x = date, y = new_cases, color = comuna)) + 
  geom_line(show.legend = FALSE) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ comuna, ncol = 1, scales = "free_y") +
  labs(x = "Fecha", y = "Casos Nuevos",
       title = "Casos confirmados de COVID-19") 

?strftime()

res <- comunas %>% 
  filter(region %in% c("Metropolitana")) %>%
  group_by(comuna) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>% 
  arrange(desc(confirmed_n)) %>%
  filter(row_number() <= 10)

res %>% 
  ggplot(aes(x = comuna, y = confirmed_n, fill = comuna)) + 
  geom_col() +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) + 
  geom_text(aes(label = confirmed_n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "Comuna", y = "Casos Totales",
       title = "Ranking Casos Totales: Región Metropolitana")
 





