

library(tidyverse)

## 1 # Data
pwd_meta <-"/home/mgcl/Escritorio/Sexto semestre/Minería de datos/Talleres/Taller 2/Data/metadata_estaciones_agrometAPI.csv"
data_info <- read.csv(pwd_meta,sep = ";")


## Elimina la columna extra de indice
data_info$X <- NULL
str(data_info)



## 2 # Ejercicios

#1
#a a que tipo de estructura de datos corresponde data_info.
str(data_info)
#Respuesta: Corresponde a un dataframe

#b cuantas observaciones tiene el set de datos
print(length(data_info$ema))
#Respuesta: Posee 417 observaciones

#c cuántas y a qué tipo de dato corresponden las variables de data_info
str(data_info)
#Respuesta: Posee 8 variables: ema (tipo entero), institucion (tipo character)
#nombre_ema (tipo character), comuna (tipo character), region (tipo character)
#latitud (tipo character), longitud (tipo character), fecha_de_alta (tipo character)

#d ¿Corresponde a un set de datos ajustado (tidy)? ¿Por qué?
#Respuesta: Si, ya que posee un nombre para cada variable (columnas de DF), un solo valor o
#dato en cada celda y cada fila del DF corresponde a una observación

####################################################################################

#2 Seleccione la estación que tiene el código 682 e indique cuál es el nombre, en qué 
#región y comuna se encuentra; y cuál son las coordenadas geográficas (latitud y longitud).

estacion <- data_info[data_info$ema == 682, ]
print(estacion)

#Respuesta: nombre: Ñirehuao, región Aysén, comuna: Coyhaique, latitud: -45,249744 y
#longitud: -71,69425

#3Cargue los datos data_estaciones_agrometAPI.rds y asignelo al objeto data_agromet. Indique:

#a) A qué tipo de estructura de datos corresponde
#b) cuantas variables y observaciones tiene
#c) ¿El set de datos tiene atributos?¿Cuáles?

pwd_data <- pwd<-"/home/mgcl/Escritorio/Sexto semestre/Minería de datos/Talleres/Taller 2/Data/data_estaciones_agrometAPI.rds"
data_agromet <- readRDS(pwd_data)

# a) 
str(data_agromet)
#Respuesta: Corresponde a la estructura de datos tibble 

#b) 
str(data_agromet)
print(length(data_agromet$station_id))
#Respuesta: Posee 13 Variables y 3573024 observaciones

#c)
attributes(data_agromet)
#Respuesta: Si, el set de datos posee atributos los cuales son:
#Las unidades de cada variable y la descripción de cada variable, ambas en formato chr

#4 Seleccione la estación con codigo 682 del set de datos data_agromet, la fecha y hora y 
#las variables de temperatura. Entregue un resumen estadístico (mínimo, 1er cuartil, 
#mediana, 3er cuartil, máximo) para todas la variables.


tb_station_682 <- data_agromet %>%
  filter(station_id %in% 682) %>%
  select(fecha_hora, temp_promedio_aire,temp_minima, temp_maxima, grados_dia)
  
summary(tb_station_682)

#5 Con el set de datos filtrado de la pregunta anterior, seleccione los datos que 
#corresponden a los meses de mayo a agosto.

station_682_may_ago <- tb_station_682 %>%
  filter(month(fecha_hora) %in% c(5, 8))
station_682_may_ago

#6 Calcule la cantidad de valores no disponibles (NA) de cada variable por día.

can_na_var <- tb_station_682 %>% group_by(fecha_hora) %>% 
  summarise(
    num_NA_tpa = sum(is.na(temp_promedio_aire)),
    num_NA_tpmax = sum(is.na(temp_maxima)),
    num_NA_tpmin = sum(is.na(temp_minima))
  )
print(can_na_var)

#7 Calcule la cantidad total de valores no disponibles (NA) por cada variable numérica 
#(todos los dias).


#8 Con los datos filtrados de la pregunta 5, calcule la temperatura máxima y mínima 
#para las tres variables de temperatura.

resultados <- station_682_may_ago %>%
  summarise(
    max_temp_promedio = max(temp_promedio_aire, na.rm = TRUE),
    min_temp_promedio = min(temp_promedio_aire, na.rm = TRUE),
    
    max_temp_minima = max(temp_minima, na.rm = TRUE),
    min_temp_minima = min(temp_minima, na.rm = TRUE),
    
    max_temp_maxima = max(temp_maxima, na.rm = TRUE),
    min_temp_maxima = min(temp_maxima, na.rm = TRUE),
    
    max_grados_dia = max(grados_dia, na.rm = TRUE),
    min_grados_dia = min(grados_dia, na.rm = TRUE)
  )

resultados


#9 La amplitud termica es la diferencia entre la temperatura máxima y mínima. 
#Calcule la amplitud termica diaria para los datos filtrados de la pregunta 5.

V_result_ampl <-tb_station_682 %>% group_by(fecha_hora) %>%
                summarise(diff_means=mean(temp_maxima, na.rm = TRUE) - mean(temp_minima, na.rm = TRUE))

print(V_result_ampl)




