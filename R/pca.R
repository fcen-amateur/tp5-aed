library(tidyverse)

analisis_de_componentes_principales <- function(df) {
  X <- data.matrix(df)
  descomposicion <- svd(cov(X)) # TODO: implementar un `cov` casero?
  autovalores <- descomposicion$d
  matriz_de_autovectores <- descomposicion$u
  assertthat::are_equal(descomposicion$u, descomposicion$v)
  nombres_componentes <- 1:ncol(X) %>% map_chr(~paste("PC", .x, sep = ""))
  varianza_total <- sum(autovalores)
  varianza_explicada <- autovalores / varianza_total
  names(varianza_explicada) <- nombres_componentes
  componentes_principales <- (X %*% matriz_de_autovectores) %>% as_tibble
  colnames(componentes_principales) = nombres_componentes
  
  return (list(
    X = componentes_principales,
    varianza_total = varianza_total,
    varianza_explicada = varianza_explicada
  ))
}
