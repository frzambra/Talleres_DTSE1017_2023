library(devtools)
install_github("ODES-Chile/agrometR")
library(agrometR)
library(tidyverse)

estaciones_agromet

ids <- c(392, 276, 471, 310, 559, 117,  10, 376, 531, 646)

#df <- get_agro_data(ids,as.POSIXct("2021-01-01 00:00"),as.POSIXct("2021-02-01 00:00"))
#print(df)
pwd<-"/home/mgcl/Escritorio/Sexto semestre/Minería de datos/Talleres/Taller 2/Data/data_estaciones_agrometAPI.rds"

df <- readRDS(pwd)

#### Ejercicio 2 ####
#Cree un script que permita calcular el promedio de las variables temp_promedio_aire, 
#temp_minima y temp_maxima; para todas las estaciones asignadas y todo el año 2021. 
#Excluya del cálculo los valores no disponibles (NA).

#precipitacion_horaria    temp_promedio_aire           temp_minima 
#0.03424198           13.99975066           13.46633701 
#temp_maxima 
#14.53560974 
################################ Respuesta ##################################

#Filtro de estaciones

estaciones <- c( 392, 276, 471, 310, 559, 117,  10, 376, 531, 646)

df_filtrado <- df %>%
  filter(station_id %in% estaciones)

# Lista de nombres de columnas
columnas <- c("precipitacion_horaria", "temp_promedio_aire", "temp_minima", "temp_maxima")

# Vector para almacenar los promedios
vec_prom <- c()

# Función mean_col (ya la tienes definida) 
#Esto nos permite solo eliminar los valores nulos de cada columna y no eliminar filas enteras
mean_col <- function(columna) {
  vector_col <- df_filtrado[[columna]]
  vec_clean <- na.omit(vector_col)
  mean_vec_clean <- mean(vec_clean)
  return(mean_vec_clean)
}

# Ciclo for para iterar sobre las columnas
for (col in columnas) {
  vec_prom[col] <- mean_col(col)
}

# Ver el resultado
print(vec_prom)

# 3

#Escriba un script que permita calcular la suma de la precipitación anual para
#cada una de las estaciones.

# Calcula la suma de precipitación anual por estación
resultado <- df_filtrado %>%
  # Extrae el año de la columna fecha_hora y crea una nueva columna llamada 'anio'
  mutate(anio = as.numeric(format(fecha_hora, "%Y"))) %>%
  group_by(station_id, anio) %>%
  # Suma 
summarise(precipitacion_anual = sum(precipitacion_horaria, na.rm = TRUE)) %>%
ungroup()
vector_resultado <- with(resultado, setNames(precipitacion_anual, as.character(station_id)))


print(vector_resultado)


# 4
#Cree un data.frame con el resultado del ejercicio anterior, debe tener dos columnas, 
#una con el identificador de la estación y otra con la suma anual de precipitación.


df_resultado <- data.frame(id = as.numeric(names(vector_resultado)), 
                           cumpre = as.numeric(vector_resultado))

print(df_resultado)

# 5 Cree un script que permita calcular el promedio de las variables temp_promedio_aire, 
#temp_minima y temp_maxima; para cada una de las estaciones asignadas y todo el año 2021. 
#Excluya del cálculo los valores no disponibles (NA). Vea la ayuda de la función aggregate

prom <- aggregate(cbind(temp_promedio_aire, temp_minima, temp_maxima) ~ station_id, 
                    data = df_filtrado, 
                    FUN = function(x) mean(x, na.rm = TRUE))

# Mostrando el resultado
print(prom)

#6 Cree un scrpt para dividir el data.frame en una lista donde cada elemento de la 
#lista debe corresponder al subset de datos de cada estación. Busque ayuda sobre 
#la función split

lista_estaciones <- split(df_filtrado, df_filtrado$station_id)
print(lista_estaciones)

#7 Cree un script para hacer un grafico de tipo boxplot para cada una de 
#las estaciones respecto a las variables temperatura promedio.

library(ggplot2)

plot <- ggplot(df_filtrado, aes(x = as.factor(station_id), y = temp_promedio_aire)) +
  geom_boxplot(width=0.1) +  
  labs(title = "Boxplot de Temperatura Promedio por Estación",
       x = "ID de la Estación",
       y = "Temperatura Promedio") +
  facet_wrap(~station_id, scales = "free_x") + 
  ylim(0, 30) + 
  theme_minimal()

print(plot)

#8

##9 Escriba un script que permita extraer el mes de enero de cada una de las 
#estaciones para las variables c("station_id", "fecha_hora", "temp_promedio_aire", 
#"precipitacion_horaria", "humed_rel_promedio", "presion_atmosferica"). Utilice la 
#lista del ejercicio 5.


# Función para extraer datos de enero y seleccionar columnas
extraer_enero <- function(df) {
  df_enero <- df_filtrado %>%
    filter(month(fecha_hora) == 1) %>% 
    select(station_id, fecha_hora, temp_promedio_aire, 
           precipitacion_horaria, humed_rel_promedio, presion_atmosferica)
  
  return(df_enero)
}

lista_enero <- lapply(lista_estaciones, extraer_enero)
print(lista_enero)

#10 Del ejercicio anterior, escriba un script que permita verificar que están los 
#31 dias del mes de enero 2021.

# Función para verificar 31 días en enero 2021
verificar_31_dias <- function(df) {
  num_dias <- df %>%
    mutate(dia = day(fecha_hora)) %>%
    summarise(num_dias_unicos = n_distinct(dia)) %>%
    pull(num_dias_unicos)
  
  return(num_dias == 31)
}

# Aplicar la función a cada estación en la lista
verificacion <- lapply(lista_enero, verificar_31_dias)

# Ver resultados
print(verificacion)

