library(tidyverse)
source("R/leer_dataset.R")
source("R/pca.R")
source("R/helpers.R")

# 1.1
# Ejercicio 1 (PCA + K-medias)

# Consideremos el dataset wine, con información sobre una muestra de diversos
# tipos de vino. Proceda a graficar en un mismo plot los scores de las dos primeras
# componentes principales del dataset, discriminando con colores en función de
# un procedimiento de clustering de k-medias con k = 3.
# Nota: es recomendable un posible primer paso de estandarización de los
# datos.

# Comentario extra de Lucas:

# 1) TP5 Ejercicio 1: está mal expresado tal vez en el enunciado pero lo aclaro
# acá, la idea es que hagan PRIMERO el clustering y luego proyecten los datos en
# un plot de dos dimensiones determinado por las dos primeras componentes
# principales, con un color distinto para cada cluster.

wine <- leer_dataset("data/wine.txt")
wine_escalado <- scale(wine)
cp <- analisis_de_componentes_principales(wine_escalado)

pc1_var <- ratio_a_porcentaje(cp$varianza_explicada[["PC1"]])
pc2_var <- ratio_a_porcentaje(cp$varianza_explicada[["PC2"]])

# TODO: clusterear con k-medias y usar esos colores en el siguiente plot:

cp$X %>%
  ggplot() +
  aes(PC1, PC2) +
  geom_point() +
  xlab(paste("PC 1", pc1_var, "varianza")) +
  ylab(paste("PC 2", pc2_var, "varianza"))

# 1.2
# Ejercicio 2 (DBSCAN)

# Considere el siguiente esquema de generación de datos:
#   1. Primer conjunto (“el planeta”), generar 100 datos en R 2 con la siguiente
#      receta:
#   (a) Generar n = 100 radios R i con distribución uniforme U (0, 1).
#   (b) Generar n = 100 ángulos θ i con distribución uniforme U (0, 2π).
#   (c) En base a eso, definir X i ∈ R 2 como X i = (R i cos(θ i ), R i sin(θ i )).

#   2. Segundo conjunto (“la órbita”), generar 100 datos en R 2 con la siguiente
#      receta:
#   (a) Generar n = 100 radios R i con distribución uniforme U (1.5, 2.5).
#   (b) Generar n = 100 ángulos θ i con distribución uniforme U (0, 2π).
#   (c) En base a eso, definir X i ∈ R 2 como X i = (R i cos(θ i ), R i sin(θ i )).

# Plotear en un mismo gráfico ambos datasets. Implemente un esquema
# de DBSCAN, con parámetros bien elegidos, para poder “separar” los dos
# conjuntos (“planeta” y “órbita”) en dos clusters.

# Comentario extra de Lucas:

# 2) TP5 Ejercicio 2: para la órbita, usen muchos puntos, quizás 1000, y
# eventualmente un épsilon pequeño, para que quede bien marcada la diferencia
# entre ambos.
