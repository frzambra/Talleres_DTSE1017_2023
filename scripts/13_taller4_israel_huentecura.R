library(readr)
library(tidyr)
library(purrr)
data <- read_rds('../data/data_agvAPI.rds')
info <- read_rds('../data/metadata_estaciones_agvAPI.rds')

# Trabaje con los datos de agromet asignados en el Taller 2.
# Resuelva los ejercicios del Taller 2 utilizando R-base y el dialecto con pipes, utilizando el pipe nativo de R


info_data <- readr::read_csv("../data/metadata_estaciones_agrometAPI.csv", sep = ";")
str(info_data)
# Cambiar comas por puntos y pasar las coordenadas a numérico
info_data$longitud <- as.numeric(gsub(",", ".", info_data$longitud))
info_data$latitud <- as.numeric(gsub(",", ".", info_data$latitud))
str(info_data)
data <- readRDS('./data/data_estaciones_agrometAPI.rds')
class(data)
str(data)

set.seed(987)
estaciones <- unique(data$station_id)

l <- lapply(1:5,sample,x=estaciones,size=10)
names(l) <- paste0('Alumno_',1:5)

lista_isra = l[[1]]

info_dataF <- info_data[info_data$ema %in% lista_isra,]
dataF <- data[data$station_id %in% lista_isra,]

print(info_dataF)
print(dataF)
# 1. (1Opts) Cree datos anidados por estación (station_id) considerando todas las variables climáticas.
nested_stations = dataF |> nest(data = -station_id)
print(nested_stations)
# 2. (10pts) De la data anidada extraiga el primer valor de la variable humedad relativa de la estación que
# se encuetra en la posición 5. como este ejemplo:


primer_valor_humedad = nested_stations |> purrr::pluck("data", 5) |> purrr::pluck("humed_rel_promedio", 1)
print(primer_valor_humedad)
# 3. (10pts) Agregue la variable precipitación extraida de la columna anidada como una variable adicional.
# Con hoist
nested_stations_add_prec = nested_stations |> tidyr::hoist(data, precipitacion_horaria = "precipitacion_horaria")
print(nested_stations_add_prec)
# 4 (10pts) Aplane (desanide) la variable de precipitación anterior.
# Con unnest
unnested_stations = nested_stations_add_prec |> tidyr::unnest(precipitacion_horaria)
print(unnested_stations)
# 5. (10pts) Haga explícitos los valores NA implícitos de precipitación. Compare la cantidad de
# observaciones con la data original.
# Con replace_na
unnested_stations_na <- unnested_stations %>%
  tidyr::replace_na(list(precipitacion_horaria = 0))
print(unnested_stations_na)

cantidad_de_observaciones <- sum(!is.na(unnested_stations_na$precipitacion_horaria))
print("Cantidad de observaciones después de reemplazar NA:")
print(cantidad_de_observaciones)

# Original
cantidad_de_observaciones_original <- nrow(dataF)
print("Cantidad de observaciones en la data original:")
print(cantidad_de_observaciones_original)

# 6. (10pts) Relice el rellenado de los valores NA de precipitación horaria tomando el valor anterior.
# Con fill
unnested_stations = unnested_stations |> tidyr::fill(precipitacion_horaria, .direction = "down")
print(unnested_stations)



# 1. (20pts) Cargue el set de datos data_agvAP1.rds en R y asignelo al objeto data. Indique lo siguiente:
directorio_agvAPI <- 'data/data_agvAPI.rds'
data_agv = readRDS(directorio_agvAPI)
# ¿a qué estructura de datos corresponde?
estructura = class(data_agv)
print('Estructura de datos:')
print(estructura)

# ¿qué cantidad de elementos tiene?
cantidad_elementos = length(data_agv)
print('Cantidad de elementos:')
print(cantidad_elementos)
# ¿a qué clase de datos corresponde cada elemento?
clase_elementos = sapply(data_agv, class)
print('Clase de datos de cada elemento:')
print(clase_elementos)
# 2. (10pts) Cargue el set de datos rds y asignelo al objeto info
directorio_info <- 'data/metadata_estaciones_agvAPI.rds'
info = readRDS(directorio_info)

# Indique a que estructura de datos corresponde.
estructura = class(info)
print('Estructura de datos:')
print(estructura)
# Conviertalo en un dato de clase tibble
info_tibble = as_tibble(info)
print('Info como tibble:')
print(info_tibble)
# 3. (50pts) Con las siguientes cinco estaciones de acuerdo al serial asignado a cada alumno, explore los
# datos almacenados para la variables de "Soil Moisture". Hay tres variables de "Soil Moisture" cada una
# corresponde a una profundidad de instalación del sensor a 30, 60 y 90cm.

#Israel
#[1] "z6-08724" "z6-10091" "002090A9" "z6-08726" "z6-10089"
filtro_serial = c("z6-08724", "z6-10091", "002090A9", "z6-08726", "z6-10089")
indexs_israel = which(info_tibble$serial %in% filtro_serial)

# • ¿En qué estructura estan almacenadas los datos de esas variables?
# • entre que fecha y hora se encuentran los datos de las cinco estaciones
# • cree un script que permita iterar en cada una de las list-column y extraiga la fecha y hora (timestamp)
# y el valor de humedad de suelo (96) para cada estación.
# • para el objeto info seleccione las cinco estaciones y sobreescribalo.
# Extraer estas estaciones del tibble data_agv por medio de los indices
data_sub = data_agv[indexs_israel]
info_sub = info[info$serial %in% filtro_serial,]
data_unida = tibble(info_sub, data = data_sub)
# Dropear filas con NA en la columna tipo
data_unida = data_unida[!is.na(data_unida$tipo),]


# Extraer cualquier variable que tenga el nombre "Soil Moisture" en la columna names
# Filtra los datos de interés y crea un nuevo dataframe
data_1_nivel <- data_unida |>
  unnest(data, data, measurements) |>
  filter(name %in% c('TEROS 10 Soil Moisture [1]', 'TEROS 10 Soil Moisture [2]', 'TEROS 10 Soil Moisture [3]'))

# Crea un nuevo dataframe para almacenar los datos en formato JSON
df <- data.frame(
  nombre = data_1_nivel$nombre, 
  fecha = data_1_nivel$timestamp,
  estacion = data_1_nivel$serial,
  lat = data_1_nivel$lat,
  lon = data_1_nivel$lon,
  tipo = data_1_nivel$tipo,
  name = data_1_nivel$name,
  description = data_1_nivel$description,
  profundidad = ifelse(data_1_nivel$name == 'TEROS 10 Soil Moisture [1]', 30,
                       ifelse(data_1_nivel$name == 'TEROS 10 Soil Moisture [2]', 60, 90)),
  value = data_1_nivel$value
) |> as_tibble()

print(df)