library(tmap)
library(sf)
library(tidyverse)

info_data <- read.csv("data/metadata_estaciones_agrometAPI.csv", sep = ";")
str(info_data)
# Cambiar comas por puntos y pasar las coordenadas a numérico
info_data$longitud <- as.numeric(gsub(",", ".", info_data$longitud))
info_data$latitud <- as.numeric(gsub(",", ".", info_data$latitud))
str(info_data)
data <- readRDS('./data/data_estaciones_agrometAPI.rds')
class(data)
str(data)

tmap_mode('view')

info_data |> st_as_sf(coords = c('longitud','latitud'),crs=4326) -> dtmap

tm_shape(dtmap) + tm_markers(clustering = FALSE)
# Mostrar mapa en vs code

set.seed(987)
estaciones <- unique(data$station_id)

l <- lapply(1:5,sample,x=estaciones,size=10)
names(l) <- paste0('Alumno_',1:5)

lista_isra = l[[1]]

# De los set de datos original debe filtrar las estaciones asignadas. Los resultados que se muestran a modo de ejemplo en los siguientes ejercicios, corresponden a los siguientes códigos de estaciones.
# Filtrar 
df_filtered = data %>% filter(station_id %in% lista_isra)

#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima; para todas las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA).
library(dplyr)

# Extraer el año de la columna fecha_hora
df_filtered <- df_filtered %>%
  mutate(year = lubridate::year(fecha_hora))

# Filtrar las filas correspondientes al año 2021
df_2021 <- df_filtered %>%
  filter(year == 2021)

# Calcular los promedios de las columnas deseadas, excluyendo los valores NA
promedios <- df_2021 %>%
  select(temp_promedio_aire, temp_minima, temp_maxima) %>%
  summarise_all(mean, na.rm = TRUE)

# Imprimir los promedios
print(promedios)


# Ejercicio 3
# Escriba un script que permita calcular la suma de la precipitación anual para cada una de las estaciones.
# Extraer el año de la columna fecha_hora
df_filtered <- df_filtered %>%
  mutate(year = lubridate::year(fecha_hora))

# limpiar df_filtered de nan
df_filtered <- df_filtered[!is.na(df_filtered$precipitacion_horaria),]


# Ejercicio 4
# Cree un data.frame con el resultado del ejercicio anterior, debe tener dos columnas, una con el identificador de la estación y otra con la suma anual de precipitación.

# Calcular la suma de la precipitación anual para cada una de las estaciones
precipitacion_anual <- df_filtered %>%
  group_by(station_id, year) %>%
  summarise(precipitacion_anual = sum(precipitacion_horaria, na.rm = TRUE))

# Imprimir la precipitación anual
print(precipitacion_anual)


# Ejercicio 5
#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima; para cada una de las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA). Vea la ayuda de la función aggregate
promedios = aggregate(cbind(temp_promedio_aire, temp_minima, temp_maxima) ~ station_id, data = df_2021, FUN = mean, na.rm = TRUE)
print(promedios)

# Ejercicio 6
# Cree un scrpt para dividir el data.frame en una lista donde cada elemento de la lista debe corresponder al subset de datos de cada estación. Busque ayuda sobre la función split
lista_estaciones = split(promedios, promedios$station_id)
print(lista_estaciones)

# Ejercicio 7 
# Cree un script para hacer un grafico de tipo boxplot para cada una de las estaciones respecto a las variables temperatura promedio.
# Boxplot de temperatura_promedio_aire por estacion
dividir_datos = split(df_filtered, df_filtered$station_id)
print(dividir_datos)
function_boxplot = function(x) {
  boxplot(x$temp_promedio_aire, main = paste0('Estacion ', x$station_id[1]))
}
lapply(dividir_datos, function_boxplot)

# Ejercicio 8
# Similar al ejercicio anterior, cree un script para hacer un grafico de tipo scatterplot para fecha_hora vs temp_promedio_aire para cada una de las estaciones.
# Scatterplot de fecha_hora vs temp_promedio_aire por estacion
function_scatterplot = function(x) {
  plot(x$fecha_hora, x$temp_promedio_aire, main = paste0('Estacion ', x$station_id[1]))
}
lapply(dividir_datos, function_scatterplot)

# Ejercicio 9
# Escriba un script que permita extraer el mes de enero de cada una de las estaciones para las variables c("station_id", "fecha_hora", "temp_promedio_aire", "precipitacion_horaria", "humed_rel_promedio", "presion_atmosferica"). Utilice la lista del ejercicio 5.
# Extraer el mes de enero de cada una de las estaciones
function_mes_enero = function(x) {
  x %>%
    filter(month(fecha_hora) == 1) %>%
    select(station_id, fecha_hora, temp_promedio_aire, precipitacion_horaria, humed_rel_promedio, presion_atmosferica)
}
mes_enero = lapply(dividir_datos, function_mes_enero)
print(mes_enero)

# Ejercicio 10
# Del ejercicio anterior, escriba un script que permita verificar que están los 31 dias del mes de enero 2021.
# Verificar que están los 31 dias del mes de enero 2021
function_verificar_enero = function(x) {
  x %>%
    mutate(dia = as.Date(fecha_hora)) %>%
    select(dia) %>%
    unique() %>%
    nrow()
}
verificar_enero = lapply(mes_enero, function_verificar_enero)
for (i in 1:length(verificar_enero)) {
  if (verificar_enero[[i]] == 31) {
    print(paste0('Estacion ', lista_estaciones[[i]]$station_id[1], ' tiene los 31 dias del mes de enero 2021'))
  } else {
    print(paste0('Estacion ', lista_estaciones[[i]]$station_id[1], ' no tiene los 31 dias del mes de enero 2021'))
    print(paste0('  tiene ', verificar_enero[[i]], ' dias'))
  }
}
