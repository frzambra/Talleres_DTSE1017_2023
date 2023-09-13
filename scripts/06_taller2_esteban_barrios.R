# Taller 2 - Esteban Barrios

library(tidyverse)

setwd("/Users/estebanbarrios/Desktop/U Mayor/Semestre_6/Mineria de datos/Taller2")

# 1.1- Leer el archivo metadata_estaciones_agrometAPI.csv
info_data <- read.csv('metadata_estaciones_agrometAPI.csv', sep = ";")
head(info_data)

# 1.1.2- Eliminar columnas "X" del df
info_data <- info_data[, !names(info_data) == "X"]
head(info_data)

# 1.1.3- Convertir variables str a numericas (columnas latitud y longitud)
info_data$latitud <- as.numeric(gsub(",", ".", info_data$latitud))
info_data$longitud <- as.numeric(gsub(",", ".", info_data$longitud))

class(info_data)
str(info_data)
head(info_data, 10)
tail(info_data, 10)

# 1.2- Leer el archivo data_estaciones_agrometAPI.rds
data <- readRDS('data_estaciones_agrometAPI.rds')
class(data)
str(data)
head(data, 10)
tail(data, 10)

# 1.3- Mapa de ubicación de las estaciones
library(tmap)
library(sf)
tmap_mode('view')

info_data |> 
  st_as_sf(coords = c('longitud','latitud'),crs=4326) ->dtmap

tm_shape(dtmap) +
  tm_markers(clustering = FALSE)

# 2- Seleccionar seed y las estaciones para cada alumno
set.seed(987)
estaciones <- unique(data$station_id)

l <- lapply(1:5,sample,x=estaciones,size=10)
names(l) <- paste0('Alumno_',1:5)
l
class(l)

# 2.1- De los set de datos original debe filtrar las estaciones asignadas (Alumno ")
estaciones_asignada <- c(619, 49, 673, 3, 592, 376, 158, 265, 206, 90)
print(estaciones_asignada)
datos_filtrados <- data[data$station_id %in% estaciones_asignada, ]
datos_filtrados

# 2.2- Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima;
# para todas las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA).
mean_temp_prom_aire <- mean(datos_filtrados$temp_promedio_aire, na.rm = TRUE)
mean_temp_min <- mean(datos_filtrados$temp_minima, na.rm = TRUE)
mean_temp_max <- mean(datos_filtrados$temp_maxima, na.rm = TRUE)
cat("Mean temp_promedio_aire:", mean_temp_prom_aire)
cat("Mean temp_minima:", mean_temp_min)
cat("Mean temp_maxima:", mean_temp_max)

# 2.3 y 2.4- Escriba un script que permita calcular la suma de la precipitación anual para cada una de las estaciones.
sum_precipitacion_anual <- aggregate(precipitacion_horaria ~ station_id, data = datos_filtrados, sum)
sum_precipitacion_anual
class(sum_precipitacion_anual)

# 2.5- Cree un script que permita calcular el promedio de las variables temp_promedio_aire, temp_minima y temp_maxima;
# para cada una de las estaciones asignadas y todo el año 2021. Excluya del cálculo los valores no disponibles (NA). Vea la ayuda de la función aggregate.
eliminados_na <- function(x) {
  mean(x, na.rm = TRUE)
}
resultados_promedios <- aggregate(. ~ station_id, datos_filtrados[, c("station_id", 
                                                                      "temp_promedio_aire", 
                                                                      "temp_minima", 
                                                                      "temp_maxima")], FUN = eliminados_na)
resultados_promedios

# 2.6- Cree un scrpt para dividir el data.frame en una lista donde cada elemento de la lista
# debe corresponder al subset de datos de cada estación. Busque ayuda sobre la función split.
estaciones <- split(datos_filtrados, datos_filtrados$station_id)
print(estaciones)
class(estaciones)

# 2.7- Cree un script para hacer un grafico de tipo boxplot 
# para cada una de las estaciones respecto a las variables temperatura promedio.
rows <- 2
columns <- 5
par(mfrow = c(rows, columns))
for (i in 1:length(estaciones)) {
  datos_estacion <- estaciones[[i]]
  boxplot(datos_estacion$temp_promedio_aire, main = paste0("Estación ID =", datos_estacion$station_id[1]), ylab = "Temperatura (°C)")
}
par(mfrow = c(1, 1))

# 2.8- Similar al ejercicio anterior, cree un script para hacer un grafico de tipo scatterplot 
# para fecha_hora vs temp_promedio_aire para cada una de las estaciones.
rows <- 2
columns <- 5
par(mfrow = c(rows, columns))
for (i in 1:length(estaciones)) {
  datos_estacion <- estaciones[[i]]
  plot(datos_estacion$fecha_hora, datos_estacion$temp_promedio_aire,
       main = paste0("Estación ID =", datos_estacion$station_id[1]),
       xlab = "Fecha y Hora", ylab = "Temperatura Promedio (°C)",
       pch = 20)
}
par(mfrow = c(1, 1))

# 2.9- Escriba un script que permita extraer el mes de enero de cada una de las estaciones para las variables
# c("station_id", "fecha_hora", "temp_promedio_aire", "precipitacion_horaria", "humed_rel_promedio","presion_atmosferica").
# Utilice la lista del ejercicio 2.6
estaciones_enero <- estaciones
eliminar_no_enero <- function(df) {
  mes <- format(df$fecha_hora, "%m")
  df <- df[mes == "01", ]
    return(df)
}
estaciones_enero <- lapply(estaciones_enero, eliminar_no_enero)
#Para ver si se eliminaron los otros meses
comparar1 <- estaciones_enero[[9]]
tail(comparar1)
comparar2 <- estaciones[[9]]
tail(comparar2)

#2.10- Verificar si hay datos para los 31 días de enero de 2021
estaciones_enero_31 <- estaciones_enero
eliminar_no_31 <- function(df) {
  dia <- format(df$fecha_hora, "%d")
  df <- df[dia == "31", ]
  return(df)
}
estaciones_enero_31 <- lapply(estaciones_enero_31, eliminar_no_31)
estaciones_enero_31
compatar3 <- estaciones_enero_31[[9]]
compatar3