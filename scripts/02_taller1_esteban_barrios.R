# Taller 1 Esteban Barrios

# 1.1
secuencia <- 20:50
media <- mean(20:60)
suma <- sum(51:91)
secuencia
media
suma

# 1.2
nums <- 1:100
result <- rep(NA, 100)
result[nums %% 3 == 0 & nums %% 5 == 0] <- "FizzBuzz"
result[nums %% 3 == 0 & !(nums %% 5 == 0)] <- "Fizz"
result[nums %% 5 == 0 & !(nums %% 3 == 0)] <- "Buzz"
result <- data.frame(numero = nums, resultado = result)
print(result)

# 1.3
a <- 0
b <- 1
fibonacci <- c(a, b)
for (i in 3:10) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
}
print(fibonacci)

# 1.4
encontrar_primos <- function(n) {
  x <- 2:n
  prim <- rep(NA, n)
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      prim[i] <- x[i]
      x[x %% x[i] == 0] <- NA
    }
  }
  return(prim[!is.na(prim)])
}
encontrar_primos(1000)

# 1.5
x <- rnorm(1000)
hist(x)

# 1.6
mi_vector <- 1:10
mi_matriz <- matrix(1:9, nrow = 3)
mi_funcion <- function(x) {
  return(x^6)
}
mi_lista <- list(
  vector = mi_vector,
  matriz = mi_matriz,
  funcion = mi_funcion
)
print(mi_lista)

# 1.7
matriz_5x4 <- matrix(1:20, nrow = 5, byrow = TRUE)
etiquetas_fila <- c("Fila 1", "Fila 2", "Fila 3")
etiquetas_columna <- c("Col 1", "Col 2", "Col 3")
matriz_etiquetas <- matrix(1:9, nrow = 3, byrow = TRUE, dimnames = list(etiquetas_fila, etiquetas_columna))
etiquetas_fila2 <- c("Fila A", "Fila B")
etiquetas_columna2 <- c("Col X", "Col Y")
matriz_columnas <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(etiquetas_fila2, etiquetas_columna2))
print(matriz_5x4)
print(matriz_etiquetas)
print(matriz_columnas)

# 1.8
valores <- c(1, 2, 3, 4, 5, 6)
dimensiones <- c(2, 3)
nombres_filas <- c("Fila 1", "Fila 2")
nombres_columnas <- c("Col A", "Col B", "Col C")
matriz <- matrix(valores, nrow = dimensiones[1], ncol = dimensiones[2],
                 dimnames = list(nombres_filas, nombres_columnas))
matriz

# 1.9
v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)
matriz <- matrix(c(v1, v2), nrow = 3, ncol = 3, byrow = TRUE)
matriz

# 1.10
empleados <- data.frame(
  genero = c("Hombre", "Mujer", "Hombre", "Mujer", "Hombre"),
  edad = c(25, 30, 35, 40, 45),
  rut = c("12345678-9", "98765432-1", "12345678-0", "98765432-2", "12345678-1"),
  direccion = c("Av. Las Condes 123", "Calle Arica 456", "Av. Providencia 789", "Calle Santa Rosa 101", "Av. Apoquindo 234"),
  profesion = c("Ingeniero", "Médico", "Abogado", "Arquitecto", "Profesor")
)
summary(empleados)


# 2.1
vector_numerico <- 1:6
vector_complejo <- c(1+2i, 2+3i, 3+4i, 4+5i, 5+6i, 6+7i)
vector_logico <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
vector_caracter <- c("a", "b", "c", "d", "e", "f")
vector_numerico
vector_complejo
vector_logico
vector_caracter

# 2.2
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)
vector_suma <- vector1 + vector2
vector_suma

# 2.3
vector_vacio <- c()
vector_vacio <- c(vector_vacio, 1, 2, 3, 4, 5)
vector_vacio

# 2.4
vector <- c(1, 2, 3, 4, 5)
suma <- sum(vector)
media <- mean(vector)
producto <- prod(vector)
suma
media
producto

# 2.5
vector <- c(1, 2, 3, NA, 5, NaN)
suma <- sum(vector[!is.na(vector)])
media <- mean(vector[!is.na(vector)])
producto <- prod(vector[!is.na(vector)])
suma
media
producto

# 2.6
vector <- c(1, 5, 3, 2, 4)
vector_ascendente <- sort(vector)
vector_descendente <- sort(vector, decreasing = TRUE)
vector_ascendente
vector_descendente

# 2.7
vector <- c(1, 2, 3, 4, 5)
elemento <- 3
elemento %in% vector

# 2.8
vector <- c(1, 2, 3, 4, 5)
vector_ordenado <- sort(vector, decreasing = TRUE)
segundo_valor_mas_alto <- vector_ordenado[2]
segundo_valor_mas_alto

# 2.9
vector <- c(1, 2, 3, 4, 5)
n <- 3
enesimo_valor_mas_alto <- vector[length(vector) - n + 1]
enesimo_valor_mas_alto

# 2.10
df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
columna <- "x"
vector <- df[, columna]
vector

# 2.11
vector_1 <- c(1, 2, 3, 4, 5)
vector_2 <- c(2, 4, 6, 8, 10)
elementos_unicos <- setdiff(vector_1, vector_2)
elementos_unicos

# 2.12
df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
columnas <- c("x", "y")
vector <- c()
for (columna in columnas) {
  vector <- c(vector, df[, columna])
}
vector

# 2.13
vector_1 <- c(1, 2, 3, 4, 5)
vector_2 <- c(2, 4, 6, 8, 10)
resultado <- vector_1[!vector_1 %in% vector_2]
resultado

# 2.14
vector <- c(1, 2, 3, 4, 5)
vector_invertido <- vector[length(vector):1]
vector
vector_invertido

# 2.15
vector <- c(1, 2, 3, 4, 5)
longitud <- length(vector)
dimension <- length(dim(vector))
longitud
dimension

# 2.16
vector <- c(1, 2, 3, 4, 5, 11, 12)
resultado <- vector > 10
resultado

# 2.17
vector <- c(1, 2, 3, 4, 5)
vector_nuevo <- vector + 3
vector
vector_nuevo

# 2.18
vector_1 <- 1:5
vector_2 <- seq(1, 5)
vector_1
vector_2


# 3.1
matriz_vacia <- matrix(NA, nrow = 3, ncol = 3)
matriz_vacia

# 3.2
numeros <- c(1, 2, 3)
matriz <- matrix(numeros, nrow = 2, ncol = 3)
matriz

# 3.3
numeros <- c(1, 2, 3, 4, 5, 6)
nombres_columnas <- c("columna1", "columna2", "columna3")
nombres_filas <- c("fila1", "fila2")
matriz <- matrix(numeros, nrow = 2, ncol = 3, dimnames = list(nombres_filas, nombres_columnas))
matriz

# 3.4
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 5, ncol = 5)
elemento1 <- matriz[2, 3]
elemento2 <- matriz[3,]
elemento3 <- matriz[, 4]
elemento1
elemento2
elemento3
matriz

# 3.5
matriz_1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
matriz_2 <- matrix(c(7, 8, 9, 12, 11, 10), nrow = 2, ncol = 3)
matriz_suma <- matriz_1 + matriz_2
matriz_resta <- matriz_1 - matriz_2
matriz_multiplicacion <- matriz_1 * matriz_2
matriz_division <- matriz_1 / matriz_2
matriz_suma
matriz_resta
matriz_multiplicacion
matriz_division

# 3.6
vectores <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
matriz <- cbind(vectores[[1]], vectores[[2]], vectores[[3]])
matriz

# 3.7
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
submatriz <- subset(matriz, matriz[, 3] > 7)
matriz
submatriz

# 3.8
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
arreglo <- as.vector(matriz)
matriz
arreglo

# 3.9
datos <- data.frame(x = c(1, 2, 3, 4, 5), y = c(10, 3, 1, 9, 8))
matriz_correlacion <- cor(datos)
matriz_correlacion

# 3.10
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
columnas <- list(matriz[, 1], matriz[, 2], matriz[, 3])
matriz
columnas

# 3.11
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
indice_max <- which(matriz == max(matriz), arr.ind = TRUE)
indice_min <- which(matriz == min(matriz), arr.ind = TRUE)
matriz
cat("Índice del valor máximo:", indice_max[1], ",", indice_max[2], "\n")
cat("Índice del valor mínimo:", indice_min[1], ",", indice_min[2], "\n")

# 3.12
matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
matriz_rotada <- t(matriz)[, rev(1:ncol(matriz))]
matriz
matriz_rotada

# 3.13
matriz1 <- matrix(c(1, 2, 3), nrow = 2, ncol = 2)
matriz2 <- matrix(c(4, 5, 6), nrow = 3, ncol = 2)
matriz_concatenada <- rbind(matriz1, matriz2)
matriz1
matriz2
matriz_concatenada


# 4.1
vector_1 <- c(1, 2, 3)
vector_2 <- c(4, 5, 6)
matriz_1 <- matrix(c(vector_1, vector_2), nrow = 3, ncol = 3, byrow = TRUE)
matriz_2 <- matrix(c(vector_2, vector_1), nrow = 3, ncol = 3, byrow = TRUE)
array <- array(c(matriz_1, matriz_2), dim = c(3, 3, 2))
array

# 4.2
array_3d <- array(1:24, dim = c(3, 4, 2))
array_3d

# 4.3 y 4.4
v1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
v2 <- c(10, 11, 12, 13, 14, 15, 16, 17, 18)
matriz <- rbind(matrix(v1, nrow = 3, byrow = TRUE), matrix(v2, nrow = 3, byrow = TRUE))
matriz[2, ]
matriz[3, 3]

# 4.5
array_1 <- array(c(1, 2, 3), dim = c(1, 3))
array_2 <- array(c(4, 5, 6), dim = c(1, 3))
array_3 <- array(c(7, 8, 9), dim = c(1, 3))
array_combinado <- rbind(array_1, array_2, array_3)
array_combinado

# 4.6
columnas <- c("columna1", "columna2", "columna3", "columna4")
filas <- c("fila1", "fila2", "fila3")
tabla1 <- c("a", "b", "c", "d")
tabla2 <- c("e", "f", "g", "h")
array <- array(c(tabla1, tabla2), dim = c(3, 4))
array

# 4.7 y 4.8
numeros_pares <- seq(from = 50, length.out = 15, by = 2)
array_2d <- array(numeros_pares, dim = c(5, 3))
array_2d


# 5.1
df <- data.frame()
df

# 5.2
x <- c(1, 2, 3, 4)
y <- c(5, 6, 7, 8)
z <- c(9, 10, 11, 12)
w <- c("a", "b", "c", "d")
df <- data.frame(x, y, z, w)
df

# 5.3
df <- data.frame(x = c(1, 243, 3, 200), y = c(5, 6, 7, 8), z = c(12, 13, 14, 15))
columna_x <- df$x
df
columna_x

# 5.4
df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(5, 6, 7, 8, 9, 10, 11, 12), z = c(9, 10, 11, 12, 13, 14, 15, 16))
filas <- c(3, 5)
columnas <- c(1, 3)
subconjunto <- df[filas, columnas]
subconjunto

# 5.5
df1 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8), z = c(9, 10, 11, 12))
df2 <- df1[, !(names(df1) %in% c("z"))]
df1
df2

# 5.6
df1 <- data.frame(x = c(1, 3, 5, 2), y = c(5, 8, 9, 7), z = c(4, 3, 2, 1))
df2 <- df1[order(df1$x, df1$y, df1$z), ]
df1
df2

# 5.7
df1 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
df2 <- data.frame(x = c(1, 2, 5, 6), z = c(9, 10, 11, 12))
union_interna <- merge(df1, df2, by = "x")
union_externa <- merge(df1, df2, by = "x", all = TRUE)
union_izquierda <- merge(df1, df2, by = "x", all.x = TRUE)
union_derecha <- merge(df1, df2, by = "x", all.y = TRUE)
df1
df2
union_interna
union_externa
union_izquierda
union_derecha

# 5.8
df <- data.frame(x = c(1, 2, NA, 4), y = c(5, 6, 7, 8))
df[is.na(df)] <- 3
df

# 5.9
df1 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
df2 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
names(df2) <- c("nuevo_x", "nuevo_y")
df1
df2

# 5.10
df1 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
df2 <- data.frame(x = c(1, 2, 3, 4), z = c(9, 10, 11, 12))
df_diferencias <- setdiff(df1, df2)
df_diferencias

# 5.11
df1 <- data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
df2 <- data.frame(x = c(1, 2, 5, 6), z = c(9, 10, 11, 12))
df_comunes <- merge(df1, df2, by = "x", all.x = FALSE)
df_comunes

# 5.12 ???
# 5.13
df <- data.frame(x = c(1, NA, NA, 4), y = c(5, 6, 7, 8))
num_na <- sum(is.na(df$x))
num_na

# 5.14 ???
# 5.15 ???


# 6.1
vec <- c(1, 2, 3, 4, 5)
mat <- matrix(1:9, nrow = 3, ncol = 3)
list_ <- list(vec, mat, list(1, 2, 3))
names(list_) <- c("vector", "matriz", "lista")
list_

# 6.2
vec <- c(1, 2, 3, 4, 5)
mat <- matrix(1:9, nrow = 3, ncol = 3)
list_ <- list(vec, mat, list(1, 2, 3))
names(list_) <- c("vector", "matriz", "lista")
list_[[1]]
list_[[2]]

# 6.3
vec <- c(1, 2, 3, 4, 5)
mat <- matrix(1:9, nrow = 3, ncol = 3)
list_ <- list(vec, mat, list(1, 2, 3))
names(list_) <- c("vector", "matriz", "lista")
list_[[length(list_) + 1]] <- "Estoy aprendiendo R"
list_[[4]]


# 6.4
list_ <- list(
  list(1, 2, 3),
  list(4, 5, 6),
  list(7, 8, 9)
)
segundo_elemento <- list_[[2]]
segundo_elemento

# 6.5
list_1 <- list(1, 2, 3)
list_2 <- list(4, 5, 6)
fusion <- c(list_1, list_2)
fusion

# 6.6
list_ <- list(1, 2, 3, 4, 5)
vector_ <- unlist(list_)
vector_

# 6.7
df_1 <- data.frame(x = 1:10, y = 11:20)
df_2 <- data.frame(x = 21:30, y = 31:40)
list_df <- list(df_1, df_2)
list_df[[1]]
list_df[[2]]

# 6.8
list_ <- list(1, 2, 3, 4, 5, 6, 7)
number_of_objects <- length(list_)
number_of_objects

# 6.9
df <- data.frame(x = 1:10, y = 11:20)
list_ <- split(df, 1:nrow(df))
list_

# 6.10
matrix_ <- matrix(1:20, nrow = 4, ncol = 5)
list_ <- as.list(matrix_)
matrix_
list_

# 6.11
# 6.12
# 6.13
# 6.14
# 6.15


# 7.1
v1 <- factor(c("A", "B", "A", "B", "A"))
levels(v1)

# 7.2
v1 <- factor(c("A", "B", "A", "B", "A"))
nivel_a_cambiar <- "A"
nivel_a_reemplazar <- "C"
levels(v1)[levels(v1) == nivel_a_cambiar] <- nivel_a_reemplazar
v1

# 7.3
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
meses_factor <- factor(meses, ordered = TRUE)
meses_factor

# 7.4
factor1 <- factor(c("A", "B", "C"))
factor2 <- factor(c("D", "E", "F"))
factor_concatenado <- c(factor1, factor2)
factor_concatenado

# 7.5
letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
muestra <- sample(letters, 20)
factor_muestra <- factor(muestra)
niveles_factor <- levels(factor_muestra)[1:5]
niveles_factor

# 7.6
alturas <- c(150, 150, 160, 150, 170, 175, 180, 180, 190, 150)
pesos <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
datos <- data.frame(altura = alturas, peso = pesos)
factor_alturas <- factor(datos$altura)
factor_alturas