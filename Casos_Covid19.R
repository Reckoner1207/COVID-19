library(tidyverse)
library(dplyr)
library(lubridate)

# 7. ¿cómo usted devolvería un vector lógico indicando con TRUE aquellos 
# valores “1” y FALSE para el resto de los valores del vector?

vector <- c(1,3,-1,2,NA,1,0)

# Esto Puede dar resultado, pero no sé como explicarlo
vector %in% TRUE

# Mejor opción
is.element(vector, 1)


# 8.	¿Es lo mismo print(1:5) de for(i in 1:5) print(i) a nivel 
#     de resultado? ¿Por qué?

print(1:5)

for (i in 1:5) print(i)

# R: Es lo mismo, porque en el primer caso mostramos un vector horizontal 
# de números que va desde el 1 hasta el 5, en cambio en el segundo caso, 
# se da el mismo vector pero cada número se muestra por separado. 


# 9.	A partir del código de más abajo, ¿existe otra manera de llegar 
# al mismo resultado? 

x <- c(1,2,3,4,5)
for(i in 1:5) 
x[i] <- x[i] + 1
x

# Esta es una forma parecida

for(i in 1:5) print(i + 1)

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

# Luego queremos saber cuando fue el primer caso de covid en Chile.
# Para eso ordenamos y agrupamos por fecha. Quitamos de la base los
# datos por región que muestran el Total y los confirmados en cero.

covid <- covid19 %>% 
  arrange(Fecha) %>% 
  group_by(Fecha, Confirmados) %>%
  filter(Fecha == min(Fecha)) %>%
  filter(Region != 'Total' & Confirmados != 0) %>%
  slice(1) %>%
  ungroup()

# ¿Que día superaron los 1000 casos en la Araucanía?

covid %>% 
  filter(Region == 'Araucanía' & Confirmados >= 1000)

# Que región reportó el mayor número de casos el 17-04-2020

covid %>%
  filter(Fecha == '2020-04-17') %>%
  arrange(desc(Confirmados))

# ¿Cuáles son las tres regiones que hasta el 1 de junio de 2020 
# tenían la mayor cantidad total de casos? 

covid %>%
  group_by(Region) %>%
  filter(Fecha >= '2020-03-03' & Fecha <= '2020-06-01') %>%
  summarise(Confirmados = sum(Confirmados)) %>%
  arrange(desc(Confirmados))

covid2 <- covid %>%
  arrange(Fecha) %>%
  group_by(Region) %>%
  mutate(Nuevos_Casos = Confirmados - lag(Confirmados, default = 0)) %>%
  ungroup()

covid2 %>% 
  filter(Region %in% c('O’Higgins', 'Los Lagos')) %>%
  ggplot(aes(x = Fecha, y = Nuevos_Casos, color = Region)) + 
  geom_line() + 
  scale_x_date(date_breaks = '1 month', date_labels = "%b %y") + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~ Region, ncol = 1, scales = "free_y") +
  labs(x = "Fecha", y = "Casos Nuevos", 
       title = "Casos Confirmados de Covid-19")
