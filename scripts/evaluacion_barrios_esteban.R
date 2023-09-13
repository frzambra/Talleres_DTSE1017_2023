#Evaluación 1 Esteban Barrios

library(readr)
library(dplyr)
#install.packages("microbenchmark")
library(microbenchmark)


# 2.1
# Fijar el directorio
setwd("/Users/estebanbarrios/Desktop/U Mayor/Semestre_6/Mineria de datos/Taller2")
data_info <- read.csv("metadata_estaciones_agrometAPI.csv", sep =";")

# Reemplazar comas por puntos
data_info$latitud <- gsub(",", ".", data_info$latitud)
data_info$longitud <- gsub(",", ".", data_info$longitud)
# Convertir las columnas de str a numeros
data_info$latitud <- as.numeric(data_info$latitud)
data_info$longitud <- as.numeric(data_info$longitud)

head(data_info)

#...a que tipo de estructura de datos corresponde data_info.
class(data_info)
#...cuantas observaciones tiene el set de datos
nrow(data_info)
#...cuántas y a qué tipo de dato corresponden las variables de data_info
str(data_info)
#...¿Corresponde a un set de datos ajustado (tidy)? ¿Por qué?
# 1- sí, es un set de datos tidy porque cada columna es una variable y cada fila una observación.
# 2- cada observación corresponde a una estación meteorológica.
# 3- cada variable mide una característica de la estación meteorológica.

# 2.2
# Seleccione la estación que tiene el código 682 e indique cuál es el nombre, en qué región y comuna se encuentra; y cuál son las coordenadas geográficas (latitud y longitud).
estacion_md <- data_info[data_info$ema == 682, ]
head(estacion_md)
print(paste("Nombre: ", estacion_md$nombre_ema))
print(paste("Región: ", estacion_md$region))
print(paste("Comuna: ", estacion_md$comuna))
print(paste("Latitud: ", estacion_md$latitud))
print(paste("Longitud: ", estacion_md$longitud))
# 2.3
# Cargue los datos data_estaciones_agrometAPI.rds y asignelo al objeto data_agromet. Indique:
data_agromet <- readRDS("data_estaciones_agrometAPI.rds")

#...A qué tipo de estructura de datos corresponde.
class(data_agromet)

#...cuantas variables y observaciones tiene
ncol(data_agromet)
nrow(data_agromet)

#...¿El set de datos tiene atributos?¿Cuáles?
attributes(data_agromet)

# 2.4
# Seleccione la estación con codigo 682 del set de datos data_agromet, la fecha y hora y las variables de temperatura. Entregue un resumen estadístico (mínimo, 1er cuartil, mediana, 3er cuartil, máximo) para todas la variables.
estacion_da <- data_agromet[data_agromet$station_id == 682, ]
head(estacion_da)
estacion_da <- estacion_da[, c("fecha_hora", "temp_promedio_aire", "temp_minima", "temp_maxima")]
head(estacion_da)
summary(estacion_da)

# 2.5
# Con el set de datos filtrado de la pregunta anterior, seleccione los datos que corresponden a los meses de mayo a agosto.
estacion_da$fecha_hora <- as.POSIXct(estacion_da$fecha_hora)
fechas_filtradas <- estacion_da %>%
  filter(format(fecha_hora, "%m") %in% c("05", "06", "07", "08"))
print(fechas_filtradas, n = 72)



# 2.6
# Calcule la cantidad de valores no disponibles (NA) de cada variable por día.
df_na <- fechas_filtradas %>%
  mutate(fecha = format(fecha_hora, "%Y-%m-%d")) %>%
  group_by(fecha) %>%
  summarise(
    na_temp_promedio_aire = sum(is.na(temp_promedio_aire)),
    na_temp_minima = sum(is.na(temp_minima)),
    na_temp_maxima = sum(is.na(temp_maxima))
  )
head(df_na)

# 2.7
# Calcule la cantidad total de valores no disponibles (NA) por cada variable numérica (todos los dias).
df_na %>%
  select(-fecha) %>%
  colSums() %>%
  print()

# 2.8
# Con los datos filtrados de la pregunta 5, calcule la temperatura máxima y mínima para las tres variables de temperatura.
fechas_filtradas_sin_na <- fechas_filtradas %>% #eliminé las filas con NA para que no aparecieran
  filter(!is.na(temp_promedio_aire) & !is.na(temp_minima) & !is.na(temp_maxima))
df_temp <- fechas_filtradas_sin_na %>%
  mutate(fecha = format(fecha_hora, "%Y-%m-%d")) %>%
  group_by(fecha) %>%
  summarise(
    max_temp_promedio_aire = max(temp_promedio_aire),
    min_temp_promedio_aire = min(temp_promedio_aire),
    max_temp_minima = max(temp_minima),
    min_temp_minima = min(temp_minima),
    max_temp_maxima = max(temp_maxima),
    min_temp_maxima = min(temp_maxima)
  ) %>%
  ungroup()
print(df_temp, n = 15)

# 2.9
# La amplitud termica es la diferencia entre la temperatura máxima y mínima. Calcule la amplitud termica diaria para los datos filtrados de la pregunta 5.
df_amplitud <- df_temp %>%
  select(fecha, min_temp_minima, max_temp_maxima) %>%
  mutate(amplitud = max_temp_maxima - min_temp_minima) %>%
  rename(temp_max = max_temp_maxima, temp_min = min_temp_minima)
head(df_amplitud)

# 2.10
# Cree su propia versión de la función apply y nombrela apply2. Esta función debe ser creada en base a ciclos for y con condicionales if. Debe funcionar para objetos de clase data.frame y debe permitir hacer calculos por filas y columnas. Compare el tiempo de ejecución con respecto a la función apply.
apply2 <- function(data, margin, func) {
  if (!is.data.frame(data)) {
    stop("El objeto no es de clase data.frame.")
  }
  
  if (margin == 1) {
    # Aplicar la función por filas
    n_rows <- nrow(data)
    result <- vector("list", length = n_rows)
    for (i in 1:n_rows) {
      result[[i]] <- func(data[i, , drop = FALSE])
    }
    result_df <- do.call(rbind, result)
  } else if (margin == 2) {
    # Aplicar la función por columnas
    n_cols <- ncol(data)
    result <- vector("list", length = n_cols)
    for (j in 1:n_cols) {
      result[[j]] <- func(data[, j, drop = FALSE])
    }
    result_df <- as.data.frame(do.call(cbind, result))
  } else {
    stop("El margen debe ser 1 (filas) o 2 (columnas).")
  }
  
  return(result_df)
}

# Crear un dfde ejemplo
data_df <- data.frame(A = 1:3000, B = 3001:6000)

func_suma <- function(x) {
  return(sum(x))
}

resultados <- microbenchmark(
  apply(data_df, 1, func_suma),
  apply2(data_df, 1, func_suma)
)

# Calcular el promedio de los tiempos de ejecución
mean_tiempo_apply <- mean(resultados$time[resultados$expr == "apply(data_df, 1, func_suma)"])
mean_tiempo_apply2 <- mean(resultados$time[resultados$expr == "apply2(data_df, 1, func_suma)"])

# Imprimir resultados 
cat("Tiempo promedio de apply: ", mean_tiempo_apply, "\n")
cat("Tiempo promedio de apply2: ", mean_tiempo_apply2, "\n")
