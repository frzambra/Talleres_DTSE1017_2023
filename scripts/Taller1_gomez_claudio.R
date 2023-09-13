
#1 ##################### Básico #########################
## 1
# Crear una secuencia de números del 20 al 50
numeros <- seq(20, 50)
print(numeros)
# Media de los números del 20 al 60
media <- mean(seq(20, 60))
print(media)
# Suma de los números del 51 al 91
suma <- sum(seq(51, 91))
print(suma)


## 2
for (i in 1:100) {
  if (i %% 3 == 0 && i %% 5 == 0) {
    print("FizzBuzz")
  } else if (i %% 3 == 0) {
    print("Fizz")
  } else if (i %% 5 == 0) {
    print("Buzz")
  } else {
    print(i)
  }
}

## 3

# Función para obtener los primeros n números de Fibonacci
fibonacci <- function(n) {
  if (n <= 1) {
    return(1)
  } else {
    return(fibonacci(n - 1) + fibonacci(n - 2))
  }
}

# Obtener los primeros 10 números de Fibonacci
for (i in 1:10) {
  print(fibonacci(i))
}

## 4


## 5
install.packages("ggplot2")
library(ggplot2)

# Establecer parámetros para la distribución normal
media <- 0
desviacion_std <- 1

# Generar y graficar la curva de campana
grafico <- ggplot(data.frame(x=c(-5, 5)), aes(x=x)) +
  stat_function(fun=dnorm, args=list(mean=media, sd=desviacion_std), aes(color="Curva de Campana")) +
  labs(title="Distribución Normal Estándar",
       x="Valor",
       y="Densidad",
       color="Leyenda") +
  theme_minimal()

print(grafico)

## 6

# Crear una lista de elementos usando vectores, matrices y funciones
lista <- list(
  "vector" = c(1, 2, 3),
  "matriz" = matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2),
  "funcion" = function(x) x^2
)

# Imprimir el contenido de la lista
print(lista)

## 7

# Crear una matriz de 5 x 4
matriz1 <- matrix(1:20, nrow = 5, ncol = 4)

# Crear una matriz de 3 x 3 con etiquetas y rellenar la matriz por filas
matriz2 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(c("Fila 1", "Fila 2", "Fila 3"), c("Columna 1", "Columna 2", "Columna 3")))

# Crear una matriz de 2 x 2 con etiquetas y rellenar la matriz por columnas
matriz3 <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = FALSE, dimnames = list(c("Fila 1", "Fila 2"), c("Columna 1", "Columna 2")))

# Imprimir las matrices
print(matriz1)
print(matriz2)
print(matriz3)

## 8

# Crear un array con un vector de valores y un vector de dimensiones
array <- array(1:12, dim = c(2, 3, 2), dimnames = list(c("Fila 1", "Fila 2"), c("Columna 1", "Columna 2", "Columna 3"), c("Matriz 1", "Matriz 2")))

# Imprimir el array
print(array)

## 9

# Crear un array con tres columnas, tres filas y dos "matrices"
matriz1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)
matriz2 <- matrix(c(10, 11, 12, 13, 14, 15, 16, 17, 18), nrow = 3, ncol = 3, byrow = TRUE)
array <- array(c(matriz1, matriz2), dim = c(3, 3, 2))

# Imprimir la matriz
print(array)

## 10

# Crear un data.frame con los detalles de los empleados
empleados <- data.frame(
  "genero" = c("Hombre", "Mujer", "Mujer", "Hombre", "Hombre"),
  "edad" = c(27, 35, 42, 31, 46),
  "rut" = c("11.111.111-1", "22.222.222-2", "33.333.333-3", "44.444.444-4", "55.555.555-5"),
  "direccion" = c("Calle 1 #123", "Calle 2 #456", "Calle 3 #789", "Calle 4 #012", "Calle 5 #345"),
  "profesion" = c("Ingeniero", "Abogada", "Médica", "Programador", "Contador")
)

# Mostrar un resumen de los datos
summary(empleados)


#2 ##################### Vectores #########################

## 1
numeros <- numeric(6)
complejos <- complex(6)
logicos <- logical(6)
caracteres <- character(6)

## 2

# Sumar dos vectores de tipo entero y longitud 3
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
resultado <- vector1 + vector2


## 3 

# Agregar valores a un vector vacío
vector <- numeric(0)
vector <- c(vector, 1, 2, 3)

## 4

# Encontrar la suma, la media y el producto de un vector
vector <- c(1, 2, 3, 4, 5)
suma <- sum(vector)
media <- mean(vector)
producto <- prod(vector)

## 5

vector <- c(2, 4, 6, NA, NaN, 8)
suma <- sum(vector, na.rm = TRUE)
media <- mean(vector, na.rm = TRUE)
producto <- prod(vector, na.rm = TRUE)

cat("Suma: ", suma, "\n")
cat("Media: ", media, "\n")
cat("Producto: ", producto, "\n")

## 6 

# Ordenar un vector en orden ascendente y descendente
vector <- c(3, 1, 4, 1, 5, 9, 2, 6, 5)
orden_ascendente <- sort(vector)
orden_descendente <- sort(vector, decreasing = TRUE)

print(orden_ascendente)
print(orden_descendente)

## 7

vector <- c(2, 4, 6, 8, 10)
elemento <- 10
# Verificar si el elemento está en el vector
esta_en_vector <- elemento %in% vector


if (esta_en_vector) {
  cat("El número", elemento, "está en el vector.\n")
} else {
  cat("El número", elemento, "NO está en el vector.\n")
}

## 8 

# Encontrar el segundo valor más alto en un vector dado
vector <- c(1, 2, 3, 4, 5)
segundo_valor <- sort(unique(vector), decreasing = TRUE)[2]
print(segundo_valor)

## 9

# Encontrar el enésimo valor más alto en un vector dado
vector <- c(1, 2, 3, 4, 5)
n <- 3
enesimo_valor <- sort(unique(vector), decreasing = TRUE)[n]
print(enesimo_valor)

## 10 

# Convertir una columna de un data.frame en un vector
datos <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
vector <- datos$a
print(vector)

## 11

# Invertir el orden del vector dado
vector <- c(1, 2, 3, 4, 5)
vector_invertido <- rev(vector)
print(vector_invertido)

## 12

# Crear un vector y encontrar su longitud y dimensión
vector <- c(1, 2, 3, 4, 5)
print(length(vector))
print(dim(vector))

## 13

# Probar si un valor de un vector es mayor que 10
vector <- c(1, 2, 3, 4, 5)
mayor_que_10 <- vector > 10
print(mayor_que_10)

## 14

# Sumar 3 a cada elemento en un vector dado
vector <- c(1, 2, 3, 4, 5)
vector_sumado <- vector + 3
print(vector)
print(vector_sumado)

## 15 

# Crear un vector usando el operador : y la función seq()
vector1 <- 1:10
vector2 <- seq(from = 1, to = 10, by = 1)
print(vector1)
print(vector2)


#3 ##################### Matrices #########################

## 1
# Crear una matriz en blanco
matriz_blanca <- matrix(nrow = 0, ncol = 0)
print(matriz_blanca)

## 2
# Crear una matriz a partir de un vector
vector <- c(1, 2, 3, 4, 5, 6)
matriz <- matrix(vector, nrow = 2, ncol = 3)
print(matriz)

## 3
# Crear matriz con nombres para las filas y las columnas
filas <- c("fila1", "fila2")
columnas <- c("col1", "col2", "col3")
matriz_nombres <- matrix(vector, nrow = 2, dimnames = list(filas, columnas))
print(matriz_nombres)

## 4
# Acceder a elementos específicos de una matriz
matriz_ejemplo <- matrix(1:12, nrow = 4, ncol = 3)
print(matriz_ejemplo[3,])  # Toda la 3ª fila
print(matriz_ejemplo[,4])  # Toda la 4ª columna
print(matriz_ejemplo[2,3]) # Elemento en la 3ª columna y 2ª fila

## 5
# Operaciones con dos matrices
matriz1 <- matrix(c(1,2,3,4,5,6), nrow = 2)
matriz2 <- matrix(c(7,8,9,10,11,12), nrow = 2)
suma <- matriz1 + matriz2
resta <- matriz1 - matriz2
multi <- matriz1 * matriz2
division <- matriz1 / matriz2
print(suma)
print(resta)
print(multi)
print(division)

## 6
# Crear una matriz a partir de una lista de vectores
lista_vectores <- list(c(1,2,3), c(4,5,6))
matriz_lista <- matrix(unlist(lista_vectores), ncol = 3, byrow = TRUE)
print(matriz_lista)

## 7
# Extraer submatriz
matriz_dada <- matrix(c(10,4,2,8,15,7,5,20,6), nrow=3)
submatriz <- matriz_dada[matriz_dada[,2] > 7,]
print(submatriz)

## 8
# Convertir una matriz en un vector
vectorizado <- as.vector(matriz_dada)
print(vectorizado)

## 9
# Matriz de correlación a partir de un data.frame
df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
correlacion <- cor(df)
print(correlacion)

## 10
# Convertir matriz en lista de vectores-columna
lista_columnas <- split(matriz_dada, seq(ncol(matriz_dada)))
print(lista_columnas)

## 11
# Índice de valor máximo y mínimo en matriz
matriz_val <- matrix(c(3,5,1,7,8,2), nrow = 2)
print("Máximo")
print(which(matriz_val == max(matriz_val), arr.ind = TRUE))
print("Minimo")
print(which(matriz_val == min(matriz_val), arr.ind = TRUE))

## 12
# Rotar matriz 90 grados
print(matriz_val)
rotada <- t(apply(matriz_val, 2, rev))
print(rotada)

## 13
# Concatenar matrices
matrizA <- matrix(1:6, nrow = 3)
matrizB <- matrix(7:12, nrow = 3)
concatenada <- rbind(matrizA, matrizB)
print(concatenada)

#4 ##################### Arrays #########################

## 1
# Crear array de dos matrices 3x3
vector1 <- c(1:9)
vector2 <- c(10:18)
array1 <- array(c(vector1, vector2), dim = c(3, 3, 2))
print(array1)

## 2
# Crear array tridimensional de 24 elementos
array2 <- array(1:24, dim = c(4,3,2))
print(array2)

## 3
# Acceder a elementos específicos del array
print(array1[2,,2]) # Segunda fila de la segunda matriz
print(array1[3,3,1]) # Elemento de la 3ª fila y 3ª columna de la 1ª matriz

## 4
# Repitiendo el ejercicio anterior ya que parece duplicado
print(array1[2,,2]) # Segunda fila de la segunda matriz
print(array1[3,3,1]) # Elemento de la 3ª fila y 3ª columna de la 1ª matriz

##5

# Crear tres matrices de ejemplo
matriz1 <- matrix(1:5, nrow = 1)
matriz2 <- matrix(6:10, nrow = 1)
matriz3 <- matrix(11:15, nrow = 1)

# Combinar las matrices en una nueva matriz
nueva_matriz <- rbind(matriz1, matriz2, matriz3)

# Imprimir la nueva matriz
print(nueva_matriz)

## 6
# Crear un array con tres vectores
col1 <- c(1,2,3)
col2 <- c(4,5,6)
col3 <- c(7,8,9)
array3 <- array(c(col1, col2, col3), dim = c(3,3,2))
print(array3)

## 7
# Crear un array 5x3 de secuencia de enteros pares mayores que 50
secuencia <- seq(52, 76, by = 2)
array_sec <- array(secuencia, dim = c(5,3))
print(array_sec)

## 8

primer_par <- ifelse(50 %% 2 == 0, 52, 51 + 1)
secuencia_pares <- seq(primer_par, by = 2, length.out = 15)
matriz <- matrix(secuencia_pares, nrow = 5, ncol = 3)

print(matriz)

#5 ##################### Dataframes #########################

## 1
# Crear un data.frame vacío
df_vacio <- data.frame()
print(df_vacio)

## 2
# Crear un data.frame a partir de cuatro vectores dados
vector1 <- c(1,2,3)
vector2 <- c(4,5,6)
vector3 <- c(7,8,9)
vector4 <- c(10,11,12)
df <- data.frame(v1 = vector1, v2 = vector2, v3 = vector3, v4 = vector4)
print(df)

## 3
# Extraer una columna específica usando el nombre
columna <- df$v1
print(columna)

## 4
# Extraer las filas 3 y 5 con las columnas 1 y 3
df_extendido <- rbind(df, data.frame(v1=c(13,14,15), v2=c(16,17,18), v3=c(19,20,21), v4=c(22,23,24)))
subdf <- df_extendido[c(3,5), c(1,3)]
print(subdf)

## 5
# Eliminar columnas por nombre
df_sin_v1_v2 <- df[, !names(df) %in% c("v1", "v2")]
print(df_sin_v1_v2)

## 6
# Ordenar un data.frame por varias columnas
df_ordenado <- df[order(df$v1, df$v3), ]
print(df_ordenado)

## 7
# Fusiones con data.frames
df1 <- data.frame(ID = c(1,2,3,4), Nombre = c("A","B","C","D"))
df2 <- data.frame(ID = c(3,4,5,6), Apellido = c("W","X","Y","Z"))

fusion_interna <- merge(df1, df2, by = "ID", all = FALSE)
print(fusion_interna)

fusion_externa <- merge(df1, df2, by = "ID", all = TRUE)
print(fusion_externa)

fusion_izquierda <- merge(df1, df2, by = "ID", all.x = TRUE)
print(fusion_izquierda)

fusion_derecha <- merge(df1, df2, by = "ID", all.y = TRUE)
print(fusion_derecha)

## 8
# Reemplazar valores NA con 3
df_con_na <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
df_sin_na <- replace(df_con_na, is.na(df_con_na), 3)
print(df_sin_na)

## 9
# Cambiar nombres de columnas
names(df) <- c("columna1", "columna2", "columna3", "columna4")
print(df)

## 10
# Filas en df1 no presentes en df2
dif_rows <- setdiff(df1, df2)
print(dif_rows)

## 11
# Elementos comunes en dos data.frames
comunes <- intersect(df1$ID, df2$ID)
print(comunes)

## 12
# Elementos que vienen solo una vez y son comunes
unicos_comunes <- setdiff(intersect(df1$ID, df2$ID), union(df1$ID, df2$ID))
print(unicos_comunes)

## 13
# Contar NA en una columna
na_count <- sum(is.na(df_con_na$a))
print(na_count)

## 14
# Elementos duplicados y filas únicas
df_duplicados <- data.frame(a = c(1,2,3,2,4,3,5))
print(duplicated(df_duplicados))
print(unique(df_duplicados))

## 15
# Dataframe airquality sin 'Solar.R' y 'Wind'
data(airquality)
airquality <- subset(airquality, select = -c(Solar.R, Wind))
print(airquality)

#6 ##################### Listas #############################################################

## 1
# Crear una lista con un vector, una matriz y otra lista
mi_lista <- list(vector = c(1, 2, 3), matriz = matrix(1:6, nrow=2), lista_interna = list(a=10, b=20))
names(mi_lista) <- c("Vec", "Mat", "List")
print(mi_lista)

## 2
# Acceder al primer y segundo elemento de la lista
print(mi_lista[[1]])
print(mi_lista[[2]])

## 3
# Agregar un elemento al final de la lista
mi_lista$otro_vector <- c(4,5,6)
print(mi_lista)

## 4
# Seleccionar el segundo elemento de una lista anidada
print(mi_lista$List[[2]])

## 5
# Fusionar dos listas en una lista
lista1 <- list(a = 1, b = 2)
lista2 <- list(c = 3, d = 4)
fusion <- c(lista1, lista2)
print(fusion)

## 6
# Convertir una lista en vector
vectorizado <- unlist(mi_lista)
print(vectorizado)

## 7
# Crear una lista de data.frames y acceder a ellos
df1 <- data.frame(a=1:3, b=4:6)
df2 <- data.frame(x=7:9, y=10:12)
lista_dfs <- list(df1=df1, df2=df2)
print(lista_dfs$df1)
print(lista_dfs$df2)

## 8
# Contar el número de objetos en una lista
numero_objetos <- length(mi_lista)
print(numero_objetos)

## 9
# Convertir un data.frame en una lista por filas
df3 <- data.frame(a=1:3, b=4:6)
lista_df <- split(df3, seq(nrow(df3)))
print(lista_df)

## 10
# Convertir una matriz en una lista
mat <- matrix(1:6, nrow=2)
lista_mat <- split(mat, row(mat))
print(lista_mat)

## 11
# Asignar NULL a un elemento de lista
mi_lista$Vec <- NULL
print(mi_lista)

## 12
# Crear la secuencia de 15 letras mayúsculas, comenzando desde 'E'
letras_secuencia <- LETTERS[which(LETTERS == "E"):(which(LETTERS == "E") + 14)]

# Convertir en lista
s <- as.list(letras_secuencia)

# Imprimir la lista
print(s)

## 13
# Asignar nuevos nombres a los elementos de una lista
names(mi_lista) <- c("a", "b", "c")
print(mi_lista)

## 14
# Obtener la longitud de los dos primeros vectores de una lista
longitud1 <- length(mi_lista$a)
longitud2 <- length(mi_lista$b)
print(longitud1)
print(longitud2)

## 15
# Encontrar elementos no presentes en otra lista
lista3 <- list(a=1, b=2, c=3)
lista4 <- list(a=1, d=4)
diferencia <- setdiff(lista3, lista4)
print(diferencia)


#7 ##################### Factors #############################################################

## 1
# Encontrar los niveles de factor de un vector dado
vector1 <- c("Rojo", "Azul", "Verde", "Rojo", "Verde")
factor1 <- factor(vector1)
print(levels(factor1))

## 2
# Cambiar el primer nivel de un factor con otro nivel
levels(factor1)[levels(factor1) == "Azul"] <- "Amarillo"
print(factor1)

## 3
# Crear un factor ordenado a partir de los nombres de los meses
meses <- c("Enero", "Marzo", "Febrero", "Abril")
factor_meses <- factor(meses, levels = c("Enero", "Febrero", "Marzo", "Abril"), ordered = TRUE)
print(factor_meses)

## 4
# Concatenar dos factores
factor2 <- factor(c("Negro", "Blanco"))
factor_concatenado <- factor(c(as.character(factor1), as.character(factor2)))
print(factor_concatenado)

## 5
# Extraer los cinco niveles de factor a partir de una muestra aleatoria
random_letters <- sample(letters, 5)
factor_letters <- factor(random_letters)
print(levels(factor_letters))

## 6
# Factor correspondiente al conjunto de datos de altura de las mujeres
data(women)
factor_women <- factor(paste(women$height, women$weight))
print(factor_women)

