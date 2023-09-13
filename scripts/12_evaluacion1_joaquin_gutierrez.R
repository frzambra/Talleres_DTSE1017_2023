library(tidyverse)
library(tmap)
library(sf)
tmap_mode('view')
#==================================================Ejercicio1==================================================#
#Leer el archivo metadata_estaciones_agrometAPI.csv (paso 1)
data_info <- read.csv('metadata_estaciones_agrometAPI.csv', sep = ";")

#Eliminar columnas x(paso 2)
data_info <- data_info%>% select (-one_of ('X'))

#convertir variables str a numericas (latitud y longitud) (paso 3)
data_info$latitud <- as.numeric(gsub(",", ".", data_info$latitud))
data_info$longitud <- as.numeric(gsub(",", ".", data_info$longitud))

str(data_info)

#a que tipo de estructura de datos corresponde data_info.
cat("data_info corresponde a un: ", class(data_info), "\n")

#cuantas observaciones tiene el set de datos
cat("El set de datos contiene: ", nrow(data_info), " observaciones\n")

#cuántas y a qué tipo de dato corresponden las variables de data_info
variable_info <- list()
for (i in names(data_info)){
  variable_info[[i]] <- paste(length(unique(data_info[[i]])), class(data_info[[i]]))
}

cat("El set de datos contiene: ", ncol(data_info), " variables y son las siguientes del siguiente tipo:\n")
print(variable_info)

#¿Corresponde a un set de datos ajustado (tidy)? ¿Por qué?


#==================================================Ejercicio2==================================================#
#Seleccione la estación que tiene el código 682 e indique cuál es el nombre, 
#en qué región y comuna se encuentra; y cuál son las coordenadas geográficas (latitud y longitud).

estacion_682 <- subset(data_info, ema == 682)
cat("La estacion 682 se encuentra en la region de ", estacion_682$region,
    " en la cumuna de ", estacion_682$comuna,
    " y sus coordenadas geograficas son ", estacion_682$latitud, " latitud y ",
    estacion_682$longitud, " longitud")
#==================================================Ejercicio3==================================================#
#Cargue los datos data_estaciones_agrometAPI.rds y asignelo al objeto data_agromet. Indique:
data_agromet <- readRDS('data_estaciones_agrometAPI.rds')

str(data_agromet)
#A qué tipo de estructura de datos corresponde
cat("El objeto data_agromet corresponde a una: ", class(data_agromet), "\n")

#cuantas variables y observaciones tiene
cat("El objeto data_agromet de datos contiene ", ncol(data_agromet), " variables y ", 
    nrow(data_agromet), " observaciones.")

#¿El set de datos tiene atributos?¿Cuáles?
cat("El objeto data_agromet contiene los siguientes atributos: ")
attributes(data_agromet)

#==================================================Ejercicio4==================================================#
#Seleccione la estación con codigo 682 del set de datos data_agromet, la fecha y hora y las variables 
#de temperatura. Entregue un resumen estadístico (mínimo, 1er cuartil, mediana, 3er cuartil, máximo) 
#para todas la variables.

estacion682_agromet <- data_agromet %>%
  filter(station_id == 682) %>%
  select(fecha_hora, temp_promedio_aire, temp_minima, temp_maxima)

cat("Resumen estadistico de la variable fecha_hora: ")
print(summary(estacion682_agromet$fecha_hora))

cat("\n\nResumen estadistico de la variable temp_promedio_aire: ")
print(summary(estacion682_agromet$temp_promedio_aire))

cat("\n\nResumen estadistico de la variable temp_minima: ")
print(summary(estacion682_agromet$temp_minima))

cat("\n\nResumen estadistico de la variable temp_maxima: ")
print(summary(estacion682_agromet$temp_maxima))

#==================================================Ejercicio5==================================================#
#Con el set de datos filtrado de la pregunta anterior, 
#seleccione los datos que corresponden a los meses de mayo a agosto.
meses <- estacion682_agromet %>%
  filter(month(fecha_hora) >= 5 & month(fecha_hora) <=8)
print(meses)
#==================================================Ejercicio6==================================================#
#Calcule la cantidad de valores no disponibles (NA) de cada variable por día.

na_por_dia <- meses %>%
  group_by(date(fecha_hora)) %>%
  summarise_all(~ sum(is.na(.)))

print(na_por_dia)
#==================================================Ejercicio7==================================================#
#Calcule la cantidad total de valores no disponibles (NA) por cada variable numérica (todos los dias).
na_total <- na_por_dia %>%
  mutate(total_valor_na = rowSums(select_if(., is.numeric)))
str(na_total)
#==================================================Ejercicio8==================================================#
#Con los datos filtrados de la pregunta 5, calcule la temperatura máxima y mínima
#para las tres variables de temperatura

cat("Variable temp_promedio_aire. Minimo: ", min(meses$temp_promedio_aire, na.rm = TRUE), "\n",
    "Variable temp_promedio_aire. Maximo: ", max(meses$temp_promedio_aire, na.rm = TRUE), "\n",
    "Variable temp_minima Minimo: ", min(meses$temp_minima, na.rm = TRUE), "\n",
    "Variable temp_minima Maximo: ", max(meses$temp_minima, na.rm = TRUE), "\n",
    "Variable temp_maxima Minimo: ", min(meses$temp_maxima, na.rm = TRUE), "\n",
    "Variable temp_maxima Maximo: ", max(meses$temp_maxima, na.rm = TRUE), "\n")
#==================================================Ejercicio9==================================================#
#La amplitud termica es la diferencia entre la temperatura máxima y mínima. Calcule la amplitud termica
#diaria para los datos filtrados de la pregunta 5.

amplitud_termica <- meses %>%
  group_by(date(fecha_hora)) %>%
  mutate(amplitud_termica = temp_maxima - temp_minima) %>%
  select(fecha_hora, amplitud_termica)

print(amplitud_termica)

#==================================================Ejercicio10==================================================#
#(30pts) Cree su propia versión de la función apply y nombrela apply2.
#Esta función debe ser creada en base a ciclos for y con condicionales if.
#Debe funcionar para objetos de clase data.frame y debe permitir hacer calculos por filas y columnas. 
#Compare el tiempo de ejecución con respecto a la función apply. 

apply2 <- function(x, MARGIN, FUN){
  if (!is.data.frame(x)){
    stop("No es un dataframe")
  }
  
  nrow = nrow(x)
  ncol = ncol(x)
  columnas_numericas <- sapply(x, is.numeric)
  
  for (columnas in names(x[columnas_numericas])){
    if (MARGIN == 1){
      resultado <- vector("list", length=nrow)
      for (i in 1:nrow){
        resultado[i] <- FUN(x[i, , drop = FALSE])
      }
    } else if (MARGIN == 2){
      resultado <- vector("list", length=ncol)
      for (j in 1:ncol){
        resultado[j] <- FUN(x[, j, drop = FALSE])
      }
    }
  }
  return(resultado)
}
  
x <- data.frame(A = c(1,2,3), B = c(4,5,6), C = c(7,8,9))
print(apply2(x, MARGIN = 1, FUN = sum))
  


