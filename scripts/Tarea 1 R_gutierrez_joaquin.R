#Escriba un programa en R para crear una secuencia de números del 20 al 50 y encuentre la media de los números del 20 al 60 y la suma de los números del 51 al 91.

lo_basico.1 <- function(){
  secuencia <- 20:50
  media_20_60 <- mean(20:60)
  suma_51_91 <- sum(51:91)
  return(list(secuencia = secuencia, media_20_60 = media_20_60, suma_51_91 = suma_51_91))
}
lo_basico.1()

#Escriba un programa R para imprimir los números del 1 al 100 e imprime “Fizz” para múltiplos de 3, imprime “Buzz” para múltiplos de 5 e imprime “FizzBuzz” para múltiplos de ambos.
lo_basico.2 <- function(n) {
  for (i in 1:n) {
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
}
lo_basico.2(100)

#Escriba un programa R para obtener los primeros 10 números de Fibonacci.
lo_basico.3 <- function(n) {
  fibonacci <- numeric(n)
  fibonacci[1] <- 0
  fibonacci[2] <- 1
  
  for (i in 3:n) {
    fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
  }
  return(fibonacci)
}

lo_basico.3(30)

#Escriba un programa en R para obtener todos los números primos hasta un número dado (basado en la criba de Eratóstenes).
lo_basico.4 <- function(n) {
  sieve <- rep(TRUE, n)
  sieve[1] <- FALSE
  
  for (i in 2:sqrt(n)) {
    if (sieve[i]) {
      sieve[i * i:n] <- FALSE
    }
  }
  
  return(which(sieve))
}

lo_basico.4(40)

#Escriba un programa en R para crear una curva de campana de una distribución normal aleatoria
lo_basico.5 <- function(media, std, tamano) {
  random_values <- rnorm(tamano, mean = media, sd = std)
  hist(random_values, breaks = 30, probability = TRUE)
  curve(dnorm(x, mean = media, sd = std), add = TRUE, col = "red")
}

lo_basico.5(0, 1, 1000)

#Escriba un programa R para crear una lista de elementos usando vectores, matrices y funciones. Imprime el contenido de la lista.
lo_basico.6 <- function(){
  
  
}

#Escriba un programa en R para crear una matriz de 5 x 4, una matriz de 3 x 3 con etiquetas y rellene la matriz por filas y una matriz de 2 x 2 con etiquetas y rellene la matriz por columnas.
lo_basico.7 <- function(rows, cols) {
  matriz <- matrix(nrow = rows, ncol = cols)
  value <- 1
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      matriz[i, j] <- value
      value <- value + 1
    }
  }
  
  return(matriz)
}

lo_basico.7(5,4)
#Escriba un programa en R para crear una matriz, pasando un vector de valores y un vector de dimensiones. También proporcione nombres para cada dimensión.
lo_basico.8 <- function(){
  
  
}
#Escriba un programa R para crear una matriz con tres columnas, tres filas y dos “matrices”, tomando dos vectores como entrada para la matriz. Imprime la matriz.
lo_basico.9 <- function(vec1, vec2) {
  matriz <- matrix(nrow = 3, ncol = 3)
  
  matriz[, 1] <- vec1
  matriz[, 2] <- vec2
  
  return(matriz)
}

lo_basico.9(c(1, 2, 3), c(1, 2, 3))
print(combined_matrix)
#Escriba un programa R para crear un data.frame que contenga detalles (genero, edad, rut, dirección, profesión) de 5 empleados y muestre un resumen de los datos.
lo_basico.10 <- function(){
  genero <- c("Male", "Female", "Male", "Female", "Male")
  edad <- c(30, 25, 40, 35, 28)
  rut <- c("1234567-8", "9876543-2", "5678901-5", "3456789-0", "2109876-5")
  direccion <- c("Street A", "Street B", "Street C", "Street D", "Street E")
  profesion <- c("Engineer", "Doctor", "Teacher", "Artist", "Programmer")
  
  employees_df <- data.frame(Gender = genero, Age = edad, RUT = rut, Address = direccion, Profession = profesion)
  return(employees_df)
}
lo_basico.10()


#Vectores.....................

#Escriba un programa en R para crear un vector de un tipo y una longitud específicos. Cree vectores de tipos numéricos, complejos, lógicos y de caracteres de longitud 6.
vector_1 <- function() {
  vector_numerico <- numeric(6)
  vector_complejo <- complex(real = 1:6, imaginary = 6:1)
  vector_logico <- logical(6)
  vector_caracter <- letters[1:6]
  
  return(list(numerico = vector_numerico, complejo = vector_complejo, logico = vector_logico, caracter = vector_caracter))
}

resultado_1 <- vector_1()
print(resultado_1$numerico)
print(resultado_1$complejo)
print(resultado_1$logico)
print(resultado_1$caracter)

#Escriba un programa en R para sumar dos vectores de tipo entero y longitud 3.
vector_2 <- function() {
  vector1 <- c(1, 2, 3)
  vector2 <- c(4, 5, 6)
  resultado <- vector1 + vector2
  return(resultado)
}

resultado_2 <- vector_2()
print(resultado_2)

#Escriba un programa R para agregar valores a un vector vacío dado.
vector_3 <- function() {
  vector_vacio <- numeric(0)
  vector_nuevo <- c(vector_vacio, 7, 8, 9)
  return(vector_nuevo)
}

resultado_3 <- vector_3()
print(resultado_3)

#Escriba un programa en R para encontrar la suma, la media y el producto de un vector.
vector_4 <- function() {
  vector <- c(2, 4, 6, 8, 10)
  suma <- sum(vector)
  media <- mean(vector)
  producto <- prod(vector)
  
  return(list(suma = suma, media = media, producto = producto))
}

resultado_4 <- vector_4()
print(resultado_4$suma)
print(resultado_4$media)
print(resultado_4$producto)


#Escriba un programa R para encontrar la suma, la media y el producto de un vector, ignore elementos como NA o NaN.
vector_5 <- function() {
  vector <- c(2, 4, NA, 8, 10)
  suma <- sum(vector, na.rm = TRUE)
  media <- mean(vector, na.rm = TRUE)
  producto <- prod(vector, na.rm = TRUE)
  
  return(list(suma = suma, media = media, producto = producto))
}

resultado_5 <- vector_5()
print(resultado_5$suma)
print(resultado_5$media)
print(resultado_5$producto)

#Escriba un programa en R para ordenar un Vector en orden ascendente y descendente.
vector_6 <- function() {
  vector <- c(5, 1, 9, 3, 7)
  orden_ascendente <- sort(vector)
  orden_descendente <- sort(vector, decreasing = TRUE)
  
  return(list(ascendente = orden_ascendente, descendente = orden_descendente))
}

resultado_6 <- vector_6()
print(resultado_6$ascendente)
print(resultado_6$descendente)

#Escriba un programa R para probar si un vector dado contiene un elemento específico.
vector_7 <- function(elemento_buscar) {
  vector <- c(3, 6, 9, 12, 15)
  contiene_elemento <- elemento_buscar %in% vector
  return(contiene_elemento)
}

elemento_a_buscar <- 9
resultado_7 <- vector_7(elemento_a_buscar)
print(resultado_7)

#Escriba un programa R para encontrar el segundo valor más alto en un vector dado. Haga clic en mí para ver la solución de muestra
vector_8 <- function() {
  vector <- c(7, 2, 10, 5, 9)
  segundo_maximo <- max(vector[vector != max(vector)])
  return(segundo_maximo)
}

resultado_8 <- vector_8()
print(resultado_8)

#Escriba un programa en R para encontrar el enésimo valor más alto en un vector dado.
vector_9 <- function(n) {
  vector <- c(7, 2, 10, 5, 9)
  if (n > length(vector)) {
    return("N es mayor que la longitud del vector")
  }
  
  valores_ordenados <- sort(vector, decreasing = TRUE)
  enesimo_maximo <- valores_ordenados[n]
  
  return(enesimo_maximo)
}

n_valor <- 3
resultado_9 <- vector_9(n_valor)
print(resultado_9)

#Escriba un programa en R para convertir la(s) columna(s) dada(s) de un data.frame en un vector.
vector_10 <- function(dataframe, columnas) {
  vector_resultado <- unlist(dataframe[, columnas])
  return(vector_resultado)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
columnas_a_convertir <- c("A", "B")
resultado_10 <- vector_10(dataframe_ejemplo, columnas_a_convertir)
print(resultado_10)

#Escriba un programa R para encontrar los elementos de un vector dado que no están en otro vector dado.
vector_11 <- function(vector1, vector2) {
  elementos_faltantes <- setdiff(vector1, vector2)
  return(elementos_faltantes)
}

vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(3, 4, 5, 6, 7)
resultado_11 <- vector_11(vector1, vector2)
print(resultado_11)

#Escriba un programa en R para invertir el orden del vector dado. Haga clic en mí para ver la solución de muestra
vector_12 <- function(vector) {
  vector_invertido <- rev(vector)
  return(vector_invertido)
}

vector_original <- c(10, 20, 30, 40, 50)
resultado_12 <- vector_12(vector_original)
print(resultado_12)

#Escriba un programa en R para crear un vector y encuentre la longitud y la dimensión del vector.
vector_13 <- function() {
  vector1 <- 1:10
  vector2 <- seq(from = 1, to = 10, by = 2)
  return(list(op_colon = vector1, funcion_seq = vector2))
}

resultado_13 <- vector_13()
print(resultado_13$op_colon)
print(resultado_13$funcion_seq)

#Escriba un programa R para probar si el valor del elemento de un vector dado es mayor que 10 o no. Devuelve VERDADERO o FALSO.
vector_14 <- function(vector) {
  resultado <- vector > 10
  return(resultado)
}

vector_ejemplo <- c(5, 12, 8, 15, 6)
resultado_14 <- vector_14(vector_ejemplo)
print(resultado_14)


#Escriba un programa en R para sumar 3 a cada elemento en un vector dado. Imprime el vector original y el nuevo.
vector_15 <- function(vector) {
  vector_sumado <- vector + 3
  return(list(original = vector, sumado = vector_sumado))
}

vector_ejemplo <- c(2, 5, 8, 11)
resultado_15 <- vector_15(vector_ejemplo)
print(resultado_15$original)
print(resultado_15$sumado)

#Escriba un programa en R para crear un vector usando el operador : y la función seq().
vector_15 <- function(vector) {
  vector_sumado <- vector + 3
  return(list(original = vector, sumado = vector_sumado))
}

vector_ejemplo <- c(2, 5, 8, 11)
resultado_15 <- vector_15(vector_ejemplo)
print(resultado_15$original)
print(resultado_15$sumado)

#Matrices...............

#Escriba un programa en R para crear una matriz en blanco.
matriz_1 <- function(filas, columnas) {
  matriz_vacia <- matrix(nrow = filas, ncol = columnas)
  return(matriz_vacia)
}

resultado_1 <- matriz_1(3, 4)
print(resultado_1)

#Escriba un programa en R para crear una matriz tomando como entrada un vector dado de números. Muestre la matriz.
matriz_2 <- function(vector, filas, columnas) {
  matriz_creada <- matrix(vector, nrow = filas, ncol = columnas)
  return(matriz_creada)
}

vector_datos <- c(1, 2, 3, 4, 5, 6)
resultado_2 <- matriz_2(vector_datos, 2, 3)
print(resultado_2)

#Escriba un programa en R para crear una matriz que tome un vector dado de números como entrada y defina los nombres de columna y fila. Muestre la matriz.
matriz_3 <- function(vector, filas, columnas, nombres_filas, nombres_columnas) {
  matriz_creada <- matrix(vector, nrow = filas, ncol = columnas, dimnames = list(nombres_filas, nombres_columnas))
  return(matriz_creada)
}

vector_datos <- c(1, 2, 3, 4, 5, 6)
nombres_filas <- c("Fila1", "Fila2")
nombres_columnas <- c("Col1", "Col2", "Col3")
resultado_3 <- matriz_3(vector_datos, 2, 3, nombres_filas, nombres_columnas)
print(resultado_3)

#Escriba un programa en R para acceder al elemento en la 3ª columna y la 2ª fila, sólo en la 3ª fila y sólo en la 4ª columna de una matriz dada.
matriz_4 <- function(matriz) {
  elemento_3_2 <- matriz[2, 3]
  tercera_fila <- matriz[3, ]
  cuarta_columna <- matriz[, 4]
  
  return(list(elemento_3_2 = elemento_3_2, tercera_fila = tercera_fila, cuarta_columna = cuarta_columna))
}

matriz_ejemplo <- matrix(1:12, nrow = 3)
resultado_4 <- matriz_4(matriz_ejemplo)
print(resultado_4$elemento_3_2)
print(resultado_4$tercera_fila)
print(resultado_4$cuarta_columna)

#Escriba un programa en R para crear dos matrices de 2x3, luego sume, reste, multiplique y divida las matrices.
matriz_5 <- function(matriz1, matriz2) {
  suma <- matriz1 + matriz2
  resta <- matriz1 - matriz2
  multiplicacion <- matriz1 * matriz2
  division <- matriz1 / matriz2
  
  return(list(suma = suma, resta = resta, multiplicacion = multiplicacion, division = division))
}

matriz1 <- matrix(1:6, nrow = 2)
matriz2 <- matrix(7:12, nrow = 2)
resultado_5 <- matriz_5(matriz1, matriz2)
print(resultado_5$suma)
print(resultado_5$resta)
print(resultado_5$multiplicacion)
print(resultado_5$division)

#Escriba un programa en R para crear una matriz a partir de una lista de vectores dados.
matriz_6 <- function(lista_vectores) {
  matriz_creada <- do.call(cbind, lista_vectores)
  return(matriz_creada)
}

lista_vectores <- list(c(1, 2, 3), c(4, 5, 6))
resultado_6 <- matriz_6(lista_vectores)
print(resultado_6)

#Escriba un programa en R para extraer la submatriz cuyas filas tienen un valor de columna > 7 de una matriz dada.
matriz_7 <- function(matriz) {

  
}


#Escriba un programa en R para convertir una matriz en un arreglo unidimensional.
matriz_8 <- function(matriz) {
  vector_unidimensional <- as.vector(matriz)
  return(vector_unidimensional)
}

matriz_ejemplo <- matrix(1:6, nrow = 2)
resultado_8 <- matriz_8(matriz_ejemplo)
print(resultado_8)

#Escriba un programa en R para crear una matriz de correlación a partir de un marco de datos del mismo tipo de datos.
matriz_9 <- function(dataframe) {
  matriz_correlacion <- cor(dataframe)
  return(matriz_correlacion)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
resultado_9 <- matriz_9(dataframe_ejemplo)
print(resultado_9)

#Escriba un programa en R para convertir una matriz dada en una lista de columnas-vectores.
matriz_10 <- function(matriz) {
  lista_columnas <- lapply(1:ncol(matriz), function(i) matriz[, i])
  return(lista_columnas)
}

matriz_ejemplo <- matrix(1:6, nrow = 2)
resultado_10 <- matriz_10(matriz_ejemplo)
print(resultado_10)

#Escriba un programa R para encontrar el índice de fila y columna de valor máximo y mínimo en una matriz dada.
matriz_11 <- function(matriz) {
  indice_maximo <- which(matriz == max(matriz), arr.ind = TRUE)
  indice_minimo <- which(matriz == min(matriz), arr.ind = TRUE)
  
  return(list(indice_maximo = indice_maximo, indice_minimo = indice_minimo))
}

matriz_ejemplo <- matrix(c(2, 5, 8, 1, 4, 7), nrow = 2)
resultado_11 <- matriz_11(matriz_ejemplo)
print(resultado_11$indice_maximo)
print(resultado_11$indice_minimo)

#Escriba un programa en R para rotar una matriz dada 90 grados en el sentido de las agujas del reloj.
matriz_12 <- function(matriz) {
  matriz_rotada <- t(matriz)[, nrow(matriz):1]
  return(matriz_rotada)
}

matriz_ejemplo <- matrix(1:9, nrow = 3)
resultado_12 <- matriz_12(matriz_ejemplo)
print(resultado_12)

#Escriba un programa R para concatenar dos matrices dadas de la misma columna pero filas diferentes.
matriz_13 <- function(matriz1, matriz2) {
  matriz_concatenada <- rbind(matriz1, matriz2)
  return(matriz_concatenada)
}

matriz1 <- matrix(1:3, nrow = 1)
matriz2 <- matrix(4:6, nrow = 1)
resultado_13 <- matriz_13(matriz1, matriz2)
print(resultado_13)

#Arrays.........................................

#Escriba un programa en R para crear un array de dos matrices de 3x3, cada una con 3 filas y 3 columnas a partir de dos vectores dados.
array_1 <- function(vector1, vector2) {
  matriz1 <- matrix(vector1, nrow = 3, byrow = TRUE)
  matriz2 <- matrix(vector2, nrow = 3, byrow = TRUE)
  array_creado <- array(c(matriz1, matriz2), dim = c(3, 3, 2))
  return(array_creado)
}

vector1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
vector2 <- c(11, 12, 13, 14, 15, 16, 17, 18, 19)
resultado_1 <- array_1(vector1, vector2)
print(resultado_1)

#Escriba un programa en R para crear un array tridimensional de 24 elementos utilizando la función dim().
array_2 <- function() {
  array_creado <- array(1:24, dim = c(3, 4, 2))
  return(array_creado)
}

resultado_2 <- array_2()
print(resultado_2)

#Escriba un programa en R para crear un array de dos matrices de 3x3, cada una con 3 filas y 3 columnas a partir de dos vectores dados. Imprima la segunda fila de la segunda matriz del array y el elemento de la 3ª fila y la 3ª columna de la 1ª matriz.
array_4 <- function(array) {
  segunda_fila_segunda_matriz <- array[2, , 2]
  elemento_tercera_fila_tercera_columna_primera_matriz <- array[1, 3, 1]
  
  return(list(segunda_fila_segunda_matriz = segunda_fila_segunda_matriz,
              elemento_tercera_fila_tercera_columna_primera_matriz = elemento_tercera_fila_tercera_columna_primera_matriz))
}

array_ejemplo <- array(1:18, dim = c(3, 3, 2))
resultado_4 <- array_4(array_ejemplo)
print(resultado_4$segunda_fila_segunda_matriz)
print(resultado_4$elemento_tercera_fila_tercera_columna_primera_matriz)

#Escriba un programa en R para combinar tres arrays de manera que la primera fila del primer array sea seguida por la primera fila del segundo array y luego la primera fila de la tercera array.
array_5 <- function(array1, array2, array3) {
  array_combinado <- array(c(array1, array2, array3), dim = c(dim(array1)[1], dim(array1)[2], 3))
  return(array_combinado)
}

array1 <- array(1:6, dim = c(2, 3))
array2 <- array(7:12, dim = c(2, 3))
array3 <- array(13:18, dim = c(2, 3))
resultado_5 <- array_5(array1, array2, array3)
print(resultado_5)

#Escriba un programa en R para crear un array usando cuatro columnas dadas, tres filas dadas y dos tablas dadas y muestre el contenido del array.
array_6 <- function(columnas, filas, tablas) {
  vector_datos <- 1:(columnas * filas * tablas)
  array_creado <- array(vector_datos, dim = c(filas, columnas, tablas))
  return(array_creado)
}

columnas <- 3
filas <- 2
tablas <- 2
resultado_6 <- array_6(columnas, filas, tablas)
print(resultado_6)

#Escriba un programa R para crear un array bidimensional de 5x3 de secuencia de enteros pares mayores que 50.
array_7 <- function(filas, columnas) {
  vector_pares <- seq(52, by = 2, length.out = filas * columnas)
  array_creado <- array(vector_pares, dim = c(filas, columnas))
  return(array_creado)
}

filas <- 5
columnas <- 3
resultado_7 <- array_7(filas, columnas)
print(resultado_7)

#Data.frames..............................

#Escriba un programa en R para crear un data.frame vacío.
dataframe_1 <- function() {
  df_vacio <- data.frame()
  return(df_vacio)
}

resultado_1 <- dataframe_1()
print(resultado_1)
#Escriba un programa en R para crear un data.frame a partir de cuatro vectores dados.
dataframe_2 <- function(vector1, vector2, vector3, vector4) {
  df_creado <- data.frame(col1 = vector1, col2 = vector2, col3 = vector3, col4 = vector4)
  return(df_creado)
}

vector1 <- c(1, 2, 3)
vector2 <- c("A", "B", "C")
vector3 <- c(10.5, 20.2, 30.7)
vector4 <- c(TRUE, FALSE, TRUE)
resultado_2 <- dataframe_2(vector1, vector2, vector3, vector4)
print(resultado_2)
#Escriba un programa R para extraer una columna específica de un data.frame usando el nombre de la columna.
dataframe_3 <- function(dataframe, columna) {
  columna_extraida <- dataframe[[columna]]
  return(columna_extraida)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
columna_a_extraer <- "B"
resultado_3 <- dataframe_3(dataframe_ejemplo, columna_a_extraer)
print(resultado_3)
#Escriba un programa en R para extraer las filas 3 y 5 con las columnas 1 y 3 de un data.frame dado.
dataframe_4 <- function(dataframe) {
  subconjunto_filas_columnas <- dataframe[c(3, 5), c(1, 3)]
  return(subconjunto_filas_columnas)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3, 4, 5), B = c(6, 7, 8, 9, 10), C = c(11, 12, 13, 14, 15))
resultado_4 <- dataframe_4(dataframe_ejemplo)
print(resultado_4)

#Escriba un programa en R para eliminar la(s) columna(s) por nombre de un data.frame dado.
dataframe_5 <- function(dataframe, columnas_a_eliminar) {
  dataframe_limpio <- dataframe[, !(names(dataframe) %in% columnas_a_eliminar)]
  return(dataframe_limpio)
}

dataframe_ejemplo <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
columnas_a_eliminar <- c("B", "C")
resultado_5 <- dataframe_5(dataframe_ejemplo, columnas_a_eliminar)
print(resultado_5)

#Escriba un programa en R para ordenar un data.frame dado por varias columnas.
dataframe_6 <- function() {

}

#Escriba un programa en R para crear uniones (fusiones) internas, externas, izquierdas y derechas a partir de dos data.frames dados.
dataframe_7 <- function(dataframe1, dataframe2) {
  fusion_interna <- merge(dataframe1, dataframe2, by = "ID", all = FALSE)
  fusion_externa <- merge(dataframe1, dataframe2, by = "ID", all = TRUE)
  fusion_izquierda <- merge(dataframe1, dataframe2, by = "ID", all.x = TRUE)
  fusion_derecha <- merge(dataframe1, dataframe2, by = "ID", all.y = TRUE)
  
  return(list(fusion_interna = fusion_interna, fusion_externa = fusion_externa,
              fusion_izquierda = fusion_izquierda, fusion_derecha = fusion_derecha))
}

dataframe1 <- data.frame(ID = c(1, 2, 3), Valor1 = c("A", "B", "C"))
dataframe2 <- data.frame(ID = c(2, 3, 4), Valor2 = c("X", "Y", "Z"))
resultado_7 <- dataframe_7(dataframe1, dataframe2)
print(resultado_7$fusion_interna)
print(resultado_7$fusion_externa)
print(resultado_7$fusion_izquierda)
print(resultado_7$fusion_derecha)

#Escriba un programa R para reemplazar los valores de NA con 3 en un data.frame dado.
dataframe_8 <- function(dataframe) {
  dataframe_sin_na <- ifelse(is.na(dataframe), 3, dataframe)
  return(dataframe_sin_na)
}

dataframe_ejemplo <- data.frame(A = c(1, NA, 3), B = c(NA, 5, NA))
resultado_8 <- dataframe_8(dataframe_ejemplo)
print(resultado_8)

#Escriba un programa R para cambiar más de un nombre de columna de un data.frame dado.
dataframe_9 <- function(dataframe, cambios_nombres) {
  for (nombre_actual in names(dataframe)) {
    if (nombre_actual %in% cambios_nombres) {
      nuevo_nombre <- cambios_nombres[nombre_actual]
      names(dataframe)[names(dataframe) == nombre_actual] <- nuevo_nombre
    }
  }
  return(dataframe)
}

dataframe_ejemplo <- data.frame(OldName1 = c(1, 2), OldName2 = c(3, 4))
cambios_nombres <- c(OldName1 = "NewName1", OldName2 = "NewName2")
resultado_9 <- dataframe_9(dataframe_ejemplo, cambios_nombres)
print(resultado_9)

#Escriba un programa R para comparar dos data.frame para encontrar las filas en el primer data.frame que no están presentes en el segundo marco de datos.

#Escriba un programa en R para encontrar elementos que estén presentes en dos data.frames dados.

#Escriba un programa en R para encontrar elementos que vienen solo una vez y que son comunes a ambos data.frames dados.

#Escriba un programa R para contar el número de valores NA en una columna de data.frames.

#Escriba un programa en R para crear un data.frame utilizando dos vectores dados y muestre los elementos duplicados y las filas únicas de dicho marco de datos.

#Escriba un programa en R para llamar al conjunto de datos (incorporado) airquality. Elimine las variables ‘Solar.R’ y ‘Wind’ y muestre el data.frame.

#Listas......................................................