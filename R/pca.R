library(tidyverse)

analisis_de_componentes_principales <- function(df) {
  X <- data.matrix(df)
  descomposicion <- svd(cov(X)) # TODO: implementar un `cov` casero?
  autovalores <- descomposicion$d
  matriz_de_autovectores <- descomposicion$u
  assertthat::are_equal(descomposicion$u, descomposicion$v)
  nombres_componentes <- 1:ncol(X) %>% map_chr(~glue("PC{.x}"))
  varianza_total <- sum(autovalores)
  varianza_explicada <- autovalores / varianza_total
  names(varianza_explicada) <- nombres_componentes
  X_proyectada_a_los_PCs <- as_tibble(X %*% matriz_de_autovectores)
  colnames(X_proyectada_a_los_PCs) = nombres_componentes
  
  return (list(
    X = X_proyectada_a_los_PCs,
    varianza_total = varianza_total,
    varianza_explicada = varianza_explicada,
    matriz_de_autovectores = matriz_de_autovectores
  ))
}
