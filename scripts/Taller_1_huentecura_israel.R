
## Lo Básico ##
# Ejercicio 1
print("Ejercicio 1")
secuencia <- 20:50
media <- mean(20:60)
suma <- sum(51:91)

print(secuencia)
print(media)
print(suma)

# Ejercicio 2
print("Ejercicio 2")

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

# Ejercicio 3 Obtener los 10 primeros numeros de la sucesión de fibonacci
print("Ejercicio 3")

fibonacci <- function(n) {
  if (n == 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  } else {
    return(fibonacci(n - 1) + fibonacci(n - 2))
  }
}

for (i in 0:10) {
  print(fibonacci(i))
}


# Ejercicio 4 Escriba un programa en R para obtener todos los números primos hasta un número dado 
print("Ejercicio 4")

primos <- function(n) {
  i <- 2
  while (i <= n) {
    j <- 2
    while (j <= i) {
      if (i == j) {
        print(i)
      }
      if (i %% j == 0) {
        break
      }
      j <- j + 1
    }
    i <- i + 1
  }
}

primos(100)

# Ejercicio 5 Escriba un programa en R para crear una curva de campana de una distribución normal aleatoria
print("Ejercicio 5")
valores <- rnorm(1000, mean = 0, sd = 1)
hist(valores, col = "blue", main = "Curva de campana", xlab = "Valores")

# Ejercicio 6 Escriba un programa R para crear una lista de elementos usando vectores, matrices y funciones. Imprime el contenido de la lista.
print("Ejercicio 6")
print('no entendi bien el ejercicio')

# Ejercicio 7 Escriba un programa en R para crear una matriz de 5 x 4, una matriz de 3 x 3 con etiquetas y rellene la matriz por filas y una matriz de 2 x 2 con etiquetas y rellene la matriz por columnas.
print("Ejercicio 7")
matriz1 <- matrix(1:20, nrow = 5, ncol = 4)
print(matriz1)
matriz2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
print(matriz2)
matriz3 <- matrix(1:4, nrow = 2, ncol = 2, byrow = FALSE)
print(matriz3)

# Ejercicio 8 Escriba un programa en R para crear una matriz, pasando un vector de valores y un vector de dimensiones. También proporcione nombres para cada dimensión.
print("Ejercicio 8")
vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
dimensiones <- c(3, 3)
make_matrix <- function(vector, dimensiones) {
  matrix(vector, nrow = dimensiones[1], ncol = dimensiones[2])
}
print(make_matrix(vector, dimensiones))


# Ejercicio 9
# Crear una matriz con tres columnas, tres filas y dos "matrices", tomando dos vectores como entrada para la matriz. Imprimir la matriz.
print("Ejercicio 9")
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
matriz_2x3 <- matrix(c(vector1, vector2), nrow = 2, ncol = 3, byrow = TRUE)
print(matriz_2x3)


# Ejercicio 10 Escriba un programa R para crear un data.frame que contenga detalles (genero, edad, rut, dirección, profesión) de 5 empleados y muestre un resumen de los datos.
print("Ejercicio 10")
genero <- c("M", "F", "M", "F", "M")
edad <- c(20, 30, 40, 50, 60)   
rut <- c("1-1", "2-2", "3-3", "4-4", "5-5")
direccion <- c("direccion1", "direccion2", "direccion3", "direccion4", "direccion5")
profesion <- c("profesion1", "profesion2", "profesion3", "profesion4", "profesion5")
empleados <- data.frame(genero, edad, rut, direccion, profesion)
print(empleados)

# VECTORES
# Ejercicio 1
# Vectores de diferentes tipos y longitudes
num_vector <- c(1.5, 2.3, 3.7, 4.2, 5.1, 6.8)
complex_vector <- c(1 + 2i, 2 + 3i, 3 + 4i, 4 + 5i, 5 + 6i, 6 + 7i)
logical_vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
char_vector <- c("apple", "banana", "cherry", "date", "elderberry", "fig")

# Ejercicio 2
# Sumar dos vectores de tipo entero
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
suma_vector <- vector1 + vector2
print(suma_vector)
# Ejercicio 3
# Agregar valores a un vector vacío
vector_vacio <- numeric(0)
vector_vacio <- c(vector_vacio, 10, 20, 30)

# Ejercicio 4
# Calcular suma, media y producto de un vector
num_vector <- c(1, 2, 3, 4, 5, 6)
suma <- sum(num_vector)
media <- mean(num_vector)
producto <- prod(num_vector)

# Ejericio 5
# Calcular suma, media y producto de un vector, ignorando NA y NaN
num_vector <- c(1, 2, NA, 4, NaN, 6)
suma <- sum(num_vector, na.rm = TRUE)
media <- mean(num_vector, na.rm = TRUE)
producto <- prod(num_vector, na.rm = TRUE)

# Ejercicio 6
# Ordenar un vector
num_vector <- c(5, 2, 8, 1, 9, 3)
orden_ascendente <- sort(num_vector)
orden_descendente <- sort(num_vector, decreasing = TRUE)

# Ejercicio 7
# Verificar si un vector contiene un elemento
char_vector <- c("apple", "banana", "cherry", "date", "elderberry", "fig")
elemento <- "cherry"
contiene_elemento <- elemento %in% char_vector

# Ejercicio 8
# Encontrar el segundo valor más alto en un vector
num_vector <- c(5, 2, 8, 1, 9, 3)
segundo_max <- sort(num_vector, decreasing = TRUE)[2]

# Ejercicio 9

# Encontrar el enésimo valor más alto en un vector
num_vector <- c(5, 2, 8, 1, 9, 3)
n <- 3  # Cambiar el valor de n según el enésimo valor que desees encontrar
enesimo_max <- sort(num_vector, decreasing = TRUE)[n]

# Ejercicio 10

# Convertir columnas de un data.frame en vectores
data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
vector_x <- data$x
vector_y <- data$y

# Ejercicio 11

# Encontrar elementos en un vector que no están en otro vector
vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(3, 4, 5, 6, 7)
elementos_faltantes <- setdiff(vector1, vector2)

# Ejercicio 12
# Invertir el orden de un vector
num_vector <- c(1, 2, 3, 4, 5, 6)
vector_invertido <- rev(num_vector)


# Ejercicio 13
# # Encontrar longitud y dimensión de un vector
num_vector <- c(1, 2, 3, 4, 5, 6)
longitud <- length(num_vector)
dimension <- dim(num_vector)  # Los vectores no tienen dimensión, esto devolverá NULL


# Ejercicio 14

# Verificar si los elementos de un vector son mayores que 10
num_vector <- c(5, 12, 8, 15, 9, 20)
mayores_que_10 <- num_vector > 10


# Ejericio 15
# Sumar 3 a cada elemento en un vector
num_vector <- c(5, 12, 8, 15, 9, 20)
nuevo_vector <- num_vector + 3

# Ejercicio 16
# Crear un vector usando el operador : y la función seq()
vector_con_operador <- 1:10
vector_con_seq <- seq(from = 1, to = 10, by = 1)


### MATRICES ###
# Ejercicio 1
# Crear una matriz en blanco
filas <- 3
columnas <- 4
matriz_en_blanco <- matrix(numeric(0), nrow = filas, ncol = columnas)

# Ejercicio 2
# Crear una matriz a partir de un vector dado
vector <- c(1, 2, 3, 4, 5, 6)
filas <- 2
columnas <- 3
matriz <- matrix(vector, nrow = filas, ncol = columnas)

# Ejercicio 3
# Crear una matriz con nombres de fila y columna
vector <- c(1, 2, 3, 4, 5, 6)
filas <- 2
columnas <- 3
nombres_filas <- c("Fila1", "Fila2")
nombres_columnas <- c("Col1", "Col2", "Col3")
matriz_con_nombres <- matrix(vector, nrow = filas, ncol = columnas, byrow = TRUE,
                              dimnames = list(nombres_filas, nombres_columnas))

# Ejercicio 4
# Acceder a elementos en una matriz
# Crear una matriz
matriz <- matrix(1:16, nrow = 4)
elemento_3_2 <- matriz[2, 3]


fila_3 <- matriz[3, ]

columna_4 <- matriz[, 4]

# Imprimir los resultados
print("Elemento en la 3ª columna y 2ª fila:")
print(elemento_3_2)
print("Elementos en la 3ª fila:")
print(fila_3)
print("Elementos en la 4ª columna:")
print(columna_4)


# Ejercicio 5
# Operaciones matriciales básicas
matriz1 <- matrix(1:6, nrow = 2)
matriz2 <- matrix(7:12, nrow = 2)
suma <- matriz1 + matriz2
resta <- matriz1 - matriz2
producto <- matriz1 * matriz2
division <- matriz1 / matriz2

# Ejercicio 6
# Crear una matriz a partir de una lista de vectores
lista_vectores <- list(c(1, 2), c(3, 4), c(5, 6))
matriz_desde_lista <- do.call(rbind, lista_vectores)

# Ejercicio 7
# Extraer submatriz con condiciones
matriz <- matrix(1:20, nrow = 4)
submatriz <- matriz[matriz[, 2] > 7, ]

# Ejercicio 8
# Convertir matriz en arreglo unidimensional
matriz <- matrix(1:6, nrow = 2)
arreglo_unidimensional <- as.vector(matriz)

# Ejercicio 9
# Crear matriz de correlación desde un data.frame
data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
matriz_correlacion <- cor(data)

# Ejercicio 10
# Convertir matriz en lista de columnas-vectores
matriz <- matrix(1:6, nrow = 2)
lista_columnas <- as.list(matriz)

 # Ejercicio 11
 # Encontrar índices de valor máximo y mínimo
matriz <- matrix(1:9, nrow = 3)
indice_max <- which(matriz == max(matriz), arr.ind = TRUE)
indice_min <- which(matriz == min(matriz), arr.ind = TRUE)

# Ejercicio 12
# Rotar una matriz 90 grados en sentido de las agujas del reloj
matriz <- matrix(1:12, nrow = 4)
matriz_rotada <- t(matriz)[, ncol(matriz):1]

# Ejercicio 13
# Concatenar dos matrices con diferentes filas
matriz1 <- matrix(1:6, nrow = 2)
matriz2 <- matrix(7:12, nrow = 2)
matriz_concatenada <- rbind(matriz1, matriz2)

## ARRAYS ##
# Ejercicio 1
# Crear un array de dos matrices 3x3 a partir de dos vectores
vector1 <- 1:9
vector2 <- 10:18
array_2_matrices <- array(c(vector1, vector2), dim = c(3, 3, 2))

# Ejercicio 2
# Crear un array tridimensional utilizando dim()
datos <- 1:24
array_3d <- array(datos, dim = c(3, 4, 2))

# Ejercicio 3

# Imprimir "Dos vectores de diferentes longitudes:"
vector_1 <- c(1,3,4,5)
vector_2 <-  c(10,11,12,13,14,15)
print("Dos vectores de diferentes longitudes:")
print(vector_1)
print(vector_2)

# Crear un nuevo array
resultado = array(c(vector_1, vector_2), dim = c(3, 3, 2))
print("Nuevo array:")
print(resultado)

# Imprimir "La segunda fila de la segunda matriz del array:"
print("La segunda fila de la segunda matriz del array:")
print(resultado[2, , 2])

# Imprimir "El elemento en la tercera fila y tercera columna de la primera matriz:"
print("El elemento en la tercera fila y tercera columna de la primera matriz:")
print(resultado[3, 3, 1])


# Ejercicio 4
# Imprimir elementos específicos de un array de dos matrices
segunda_fila_segunda_matriz <- array_2_matrices[2, , 2]
elemento_3_3_primera_matriz <- array_2_matrices[3, 3, 1]

# Ejercicio 5
# Combinar tres arrays tomando las primeras filas de cada uno
array1 <- matrix(1:9, nrow = 3)
array2 <- matrix(10:18, nrow = 3)
array3 <- matrix(19:27, nrow = 3)
combined_array <- rbind(array1[1, ], array2[1, ], array3[1, ])

# Ejercicio 6
# Crear un array usando columnas, filas y tablas dadas
columnas <- 2
filas <- 3
tablas <- 4
datos <- 1:(columnas * filas * tablas)
array_custom <- array(datos, dim = c(filas, columnas, tablas))

# Ejercicio 7

secuencia_pares <- seq(from = 52, by = 2, length.out = 5 * 3)

matriz_2d_pares <- matrix(secuencia_pares, nrow = 5, ncol = 3, byrow = TRUE)

# Imprimir la matriz resultante
print("Matriz bidimensional 5x3 de enteros pares mayores que 50:")
print(matriz_2d_pares)


# Ejercicio 8
# Crear un array 2D de secuencia de enteros pares mayores que 50
even_sequence <- seq(from = 52, by = 2, length.out = 5 * 3)
array_2d_even <- matrix(even_sequence, nrow = 5, ncol = 3)

## DATAFRAMES ##
# Ejercicio 1

# Crear un data.frame vacío
data_vacio <- data.frame()
# Ejercicio 2

# Crear un data.frame a partir de cuatro vectores dados
vector1 <- c(1, 2, 3)
vector2 <- c("A", "B", "C")
vector3 <- c(TRUE, FALSE, TRUE)
vector4 <- c(10.5, 20.3, 15.2)
data <- data.frame(Columna1 = vector1, Columna2 = vector2,
                   Columna3 = vector3, Columna4 = vector4)
# Ejercicio 3

# Extraer una columna específica de un data.frame por nombre
columna_deseada <- data$Columna2
# Ejercicio 4

# Extraer filas 3 y 5, y columnas 1 y 3 de un data.frame dado
subset_data <- data[c(3, 5), c(1, 3)]
# Ejercicio 5

# Eliminar columna(s) por nombre de un data.frame dado
data_sin_columna <- data[, !(names(data) %in% c("Columna2", "Columna4"))]
# Ejercicio 6

# Ordenar un data.frame dado por varias columnas
data_ordenado <- data[order(data$Columna1, data$Columna3), ]
# Ejercicio 7

# Realizar uniones (fusiones) internas, externas, izquierdas y derechas de dos data.frames
data2 <- data.frame(ID = c(1, 2, 3, 4, 5), y = c("A", "B", "C", "D", "E"))
data1 <- data.frame(ID = c(1, 2, 3, 4, 5), x = c(1, 2, 3, 4, 5))
merged_inner <- merge(data1, data2, by = "ID", all = FALSE)
merged_outer <- merge(data1, data2, by = "ID", all = TRUE)
merged_left <- merge(data1, data2, by = "ID", all.x = TRUE)
merged_right <- merge(data1, data2, by = "ID", all.y = TRUE)
# Ejercicio 8

# Reemplazar valores NA con 3 en un data.frame dado
data_sin_na <- data
data_sin_na[is.na(data_sin_na)] <- 3
# Ejercicio 9

# Cambiar nombres de columna en un data.frame
names(data)[c(2, 4)] <- c("NewColumn2", "NewColumn4")
# Ejercicio 10

# Encontrar filas presentes en el primer data.frame pero no en el segundo
filas_faltantes <- setdiff(data1$ID, data2$ID)
# Ejercicio 11

# Encontrar elementos comunes en dos data.frames
elementos_comunes <- intersect(data1$ID, data2$ID)
# Ejercicio 12

# Encontrar elementos únicos en ambos data.frames
elementos_unicos <- setdiff(intersect(data1$ID, data2$ID), union(data1$ID, data2$ID))
# Ejercicio 13

# Contar valores NA en una columna de un data.frame
num_na <- sum(is.na(data$Columna1))
# Ejercicio 14

# Crear un data.frame utilizando dos vectores y mostrar elementos duplicados y filas únicas
vector1 <- c(1, 2, 3, 2, 4, 5)
vector2 <- c("A", "B", "C", "A", "D", "E")
data <- data.frame(Columna1 = vector1, Columna2 = vector2)
duplicados <- data[duplicated(data), ]
unicos <- data[!duplicated(data), ]
# Ejercicio 15

# Cargar el conjunto de datos airquality
data(airquality)

# Eliminar las variables 'Solar.R' y 'Wind'
airquality <- airquality[, !(names(airquality) %in% c("Solar.R", "Wind"))]


### LISTAS ###

# Ejercicio 1

# Crear una lista que contenga un vector, una matriz y una lista con nombres
mi_lista <- list(miVar = c(1, 2, 3), miMatriz = matrix(1:6, nrow = 2),
                 miOtraLista = list("a", "b", "c"))
# Ejercicio 2

# Acceder al primer y segundo elemento de la lista
primer_elemento <- mi_lista$miVar
segundo_elemento <- mi_lista$miMatriz
# Ejercicio 3

# Agregar un elemento al final de la lista
mi_lista$miNuevaLista <- list(10, 20, 30)
# Ejercicio 4

# Seleccionar el segundo elemento de una lista anidada
segundo_elemento_anidado <- mi_lista$miOtraLista[[2]]
# Ejercicio 5

# Fusionar dos listas en una
otra_lista <- list("x", "y", "z")
lista_fusionada <- c(mi_lista, otra_lista)
# Ejercicio 6

# Convertir una lista en vector
vector_de_lista <- unlist(mi_lista$miVar)
# Ejercicio 7

# Crear una lista de data.frames y acceder a cada uno de ellos
df1 <- data.frame(x = 1:3, y = c("A", "B", "C"))
df2 <- data.frame(a = 4:6, b = c("X", "Y", "Z"))
lista_de_dfs <- list(df1, df2)
primer_df <- lista_de_dfs[[1]]
segundo_df <- lista_de_dfs[[2]]
# Ejercicio 8

# Contar el número de objetos en una lista
num_objetos <- length(mi_lista)
# Ejercicio 9

# Convertir un data.frame en una lista por filas
df <- data.frame(x = 1:3, y = c("A", "B", "C"))
lista_por_filas <- split(df, seq(nrow(df)))
# Ejercicio 10

# Convertir una matriz en una lista
matriz <- matrix(1:6, nrow = 2)
lista_de_matriz <- as.list(matriz)
# Ejercicio 11

# Asignar NULL a un elemento de lista
mi_lista$miVar <- NULL
# Ejercicio 12

# Crear una lista con una secuencia de letras mayúsculas
letras_mayusculas <- letters[5:19]
mi_lista <- list(miSecuencia = letras_mayusculas)
# Ejercicio 13

# Asignar nuevos nombres a elementos de una lista
nombres_nuevos <- c("a", "b", "c")
lista_con_nombres <- list("A", "B", "C")
names(lista_con_nombres) <- nombres_nuevos
# Ejercicio 14

# Obtener la longitud de los dos primeros vectores de una lista
longitud_vector1 <- length(mi_lista$a)
longitud_vector2 <- length(mi_lista$b)
# Ejercicio 15

# Encontrar elementos de una lista que no están en otra lista
lista1 <- list("a", "b", "c", "d")
lista2 <- list("b", "d", "e")
elementos_faltantes <- setdiff(lista1, lista2)

## FACTORS ##

# Ejercicio 1

# Encontrar los niveles de factor de un vector dado
mi_vector <- c("A", "B", "A", "C", "B", "D")
niveles <- levels(factor(mi_vector))
# Ejercicio 2

# Cambiar el primer nivel de un factor con otro nivel de un factor dado
mi_factor <- factor(c("A", "B", "A", "C", "B", "D"))
nuevos_niveles <- levels(mi_factor)
nuevos_niveles[1] <- "E"
mi_factor <- factor(mi_factor, levels = nuevos_niveles)
# Ejercicio 3

# Crear un factor ordenado a partir de los nombres de los meses
nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                   "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
factor_meses <- factor(nombres_meses, ordered = TRUE, levels = nombres_meses)
# Ejercicio 4

# Concatenar dos factores en un solo factor
factor1 <- factor(c("A", "B", "C"))
factor2 <- factor(c("D", "E", "F"))
factor_concatenado <- factor(c(factor1, factor2))
# Ejercicio 5

# Extraer los cinco niveles de factor creados a partir de letters
muestra_aleatoria <- sample(letters, 10)
factor_muestra <- factor(muestra_aleatoria)
cinco_niveles <- levels(sample(factor_muestra, 5))
# Ejercicio 6

# Crear un factor correspondiente a datos de altura y peso de mujeres
altura <- c(160, 170, 165, 155, 180)
peso <- c(60, 70, 55, 50, 75)
data_mujeres <- data.frame(Altura = altura, Peso = peso)
factor_mujeres <- factor(data_mujeres$Altura, levels = data_mujeres$Altura)