library(tmap)
library(sf)
library(tidyverse)

info_data <- read.csv("data/metadata_estaciones_agrometAPI.csv", sep = ";")
str(info_data)
#Indique:

# a que tipo de estructura de datos corresponde data_info.
tipo_de_estructura = class(info_data)
print('la estructura de datos es: ')
print(tipo_de_estructura)

# cuantas observaciones tiene el set de datos
observaciones = nrow(info_data)
print('la cantidad de observaciones es: ')
print(observaciones)

# cuántas y a qué tipo de dato corresponden las variables de data_info
cantidad_de_variables = ncol(info_data)
print('la cantidad de variables es: ')
print(cantidad_de_variables)
# Tipo de datos que corresponden las variables
tipo_de_datos = sapply(info_data, class)
print('el tipo de datos de las variables es: ')
print(tipo_de_datos)

# ¿Corresponde a un set de datos ajustado (tidy)? ¿Por qué?
print('Si corresponde, porque cada variable corresponde a una columna y cada observacion a una fila y cada observacion está en una fila distinta')
print('')
print('Ejercicio 2')
print('')
# 2 Seleccione la estación que tiene el código 682 e indique cuál es el nombre, en qué región y comuna se encuentra; y cuál son las coordenadas geográficas (latitud y longitud).
estacion_682 <- info_data %>%
  filter(ema == 682)
print('El nombre de la estacion es: ')
print(estacion_682$nombre)
print('La region de la estacion es: ')
print(estacion_682$region)
print('La comuna de la estacion es: ')
print(estacion_682$comuna)
print('La latitud de la estacion es: ')
print(estacion_682$latitud)
print('La longitud de la estacion es: ')
print(estacion_682$longitud)


# 3  Cargue los datos data_estaciones_agrometAPI.rds y asignelo al objeto data_agromet.
print('Ejercicio 3')
data_agromet <- readRDS('./data/data_estaciones_agrometAPI.rds')
# Indique:

# A qué tipo de estructura de datos corresponde
clase_data_agromet = class(data_agromet)
print('la estructura de datos es: ')
print(clase_data_agromet)
# cuantas variables y observaciones tiene

cantidad_de_variables = ncol(data_agromet)
print('la cantidad de variables es: ')
print(cantidad_de_variables)
# ¿El set de datos tiene atributos?¿Cuáles?
print('Si, tiene atributos, son(Solo los nombres): ')
print(names(data_agromet))


# 4 Seleccione la estación con codigo 682 del set de datos data_agromet, la fecha y hora y las variables de temperatura. Entregue un resumen estadístico (mínimo, 1er cuartil, mediana, 3er cuartil, máximo) para todas la variables.
estacion_682_agromet = data_agromet %>%
  filter(station_id == 682) %>%
  select(fecha_hora, temp_promedio_aire, temp_minima, temp_maxima)
print('El resumen estadistico es: ')
estadisticas = summary(estacion_682_agromet)
print(estadisticas)
# 5 Con el set de datos filtrado de la pregunta anterior, seleccione los datos que corresponden a los meses de mayo a agosto.
de_mayor_a_agosto = estacion_682_agromet %>%
  filter(month(fecha_hora) >= 5 & month(fecha_hora) <= 8)
print('Los datos filtrados son: ')
print(de_mayor_a_agosto)

# 6 Calcule la cantidad de valores no disponibles (NA) de cada variable por día.
# Separar cada dia en un dataframe distinto la fehca está en dttm
dias = de_mayor_a_agosto %>%
  mutate(dia = as.Date(fecha_hora))
# Separar cada dia en un dataframe distinto
dias = split(dias, dias$dia)
# Calcular la cantidad de valores NA por dia
cantidad_de_na = lapply(dias, function(x) colSums(is.na(x)))
print('La cantidad de valores NA por dia es: ')
print(cantidad_de_na)


# 7  Calcule la cantidad total de valores no disponibles (NA) por cada variable numérica (todos los dias).
# Quitar la columna de fecha_hora
de_mayor_a_agosto_solo_numericas = de_mayor_a_agosto %>%
  select(-fecha_hora)
cantidad_de_na_por_variable_numerica = colSums(is.na(de_mayor_a_agosto_solo_numericas))
print('La cantidad de valores NA por variable numerica es: ')
print(cantidad_de_na_por_variable_numerica)


# 8 Con los datos filtrados de la pregunta 5, calcule la temperatura máxima y mínima para las tres variables de temperatura.
# Extraer la temperatura maxima y minima para las 3 variables
# Usar which.max() para obtener el indice de la fila con la temperatura maxima
max_func = function(x) {
  return(x[which.max(x)])
}
min_func = function(x) {
  return(x[which.min(x)])
}

temp_max= apply(de_mayor_a_agosto_solo_numericas, 2, FUN = max_func)
print('La temperatura maxima es: ')
print(temp_max)
temp_min= apply(de_mayor_a_agosto_solo_numericas, 2, FUN = min_func)
print('La temperatura minima es: ')
print(temp_min)

# 9 La amplitud termica es la diferencia entre la temperatura máxima y mínima. Calcule la amplitud termica diaria para los datos filtrados de la pregunta 5.
# Usando las maximas y minimas calculadas en el ejercicio anterior
amplitud_termica = temp_max - temp_min
print('La amplitud termica es: ')
print(amplitud_termica)
# 10 Cree su propia versión de la función apply y nombrela apply2. Esta función debe ser creada en base a ciclos for y con condicionales if. Debe funcionar para objetos de clase data.frame y debe permitir hacer calculos por filas y columnas. Compare el tiempo de ejecución con respecto a la función apply.
apply2 = function(data, MARGIN, FUN) {
  if (MARGIN == 1) {
    for (i in 1:nrow(data)) {
      FUN(data[i,])
    }
  } else if (MARGIN == 2) {
    for (i in 1:ncol(data)) {
      FUN(data[,i])
    }
  } else {
    print('MARGIN debe ser 1 o 2')
  }
}
# Comparar el tiempo de ejecucion
contar_tiempo_funcion = function(funcion, data, MARGIN, FUN) {
  tiempo_inicial = Sys.time()
  funcion(data, MARGIN, FUN)
  tiempo_final = Sys.time()
  tiempo_total = tiempo_final - tiempo_inicial
  print('El tiempo de ejecucion es: ')
  print(tiempo_total)
}
# Comparar el tiempo de ejecucion de apply
contar_tiempo_funcion(apply, de_mayor_a_agosto_solo_numericas, 1, max_func)
# Comparar el tiempo de ejecucion de apply2
contar_tiempo_funcion(apply2, de_mayor_a_agosto_solo_numericas, 1, max_func)