#1 Carga de datos
metro <- read.csv("D:/R/afluenciastc_desglosado_01_2025.csv")
names(metro)
dim(metro)
summary(metro)
library(tidyverse)
str(metro)
summary(metro)

#2 Limpieza de datos
library(dplyr)
library(stringr)

metro <- metro %>%
  mutate(linea = str_replace_all(linea, 
                                 c("Linea 1" = "Línea 1",
                                   "Linea 12" = "Línea 12",
                                   "Linea 2" = "Línea 2",
                                   "Linea 3" = "Línea 3",
                                   "Linea 4" = "Línea 4",
                                   "Linea 5" = "Línea 5",
                                   "Linea 6" = "Línea 6",
                                   "Linea 7" = "Línea 7",
                                   "Linea 8" = "Línea 8",
                                   "Linea 9" = "Línea 9",
                                   "Linea A" = "Línea A",
                                   "Linea B" = "Línea B")))
metro

metro %>%
  group_by(linea) %>%
  summarise(promedio = mean(afluencia)) %>%
  arrange(desc(promedio)) %>%
  head(5)

metro %>%
  group_by(linea) %>%
  summarise(sumaAfluencia = sum(afluencia)) %>%
  arrange(desc(sumaAfluencia)) %>%
  head(5)

# Visualizacion

## Afluencia por años
totales_por_año <- metro %>%
  group_by(anio) %>%
  summarise(suma_año = sum(afluencia))
totales_por_año
ggplot(totales_por_año) + 
  geom_col(aes(x = anio, y = suma_año))

##Afluencia por linea
totales_por_linea <- metro %>%
  group_by(linea) %>%
  summarise(suma_linea = sum(afluencia))
  
totales_por_linea

ggplot(totales_por_linea) + 
  geom_col(aes(x = linea, y = suma_linea))

##Afluencia por tipo de pago
totales_por_tipo_pago <- metro %>%
  group_by(tipo_pago) %>%
  summarise(suma_pago = sum(afluencia))

totales_por_tipo_pago

ggplot(totales_por_tipo_pago) +
  geom_col(aes(x=tipo_pago, y = suma_pago))

##Facetado

##Afluencia por año y tipo de pago
totales_por_año <- metro %>%
  group_by(anio,tipo_pago) %>%
  summarise(suma_año = sum(afluencia))
totales_por_año

ggplot(totales_por_año) + 
  geom_col(aes(x = anio, y = suma_año)) +
  facet_wrap(~tipo_pago)

estaciones <- metro %>%
  group_by(estacion) %>%
  summarise(suma_estacion = sum(afluencia)) %>%
  arrange(desc(suma_estacion))
  head(10)
estaciones
