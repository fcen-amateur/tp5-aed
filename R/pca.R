library(tidyverse)
library(stringr)

analisis_de_componentes_principales <- function(X) {
  if (is.data.frame(X)) { X <- data.matrix(X) }
  X <- scale(X)

  descomposicion <- svd(cov(X))
  autovalores <- descomposicion$d
  matriz_de_autovectores <- descomposicion$u
  assertthat::are_equal(descomposicion$u, descomposicion$v)
  nombres_componentes <- stringr::str_c("PC", 1:ncol(X))
  varianza_total <- sum(autovalores)
  varianza_explicada <- autovalores / varianza_total
  names(varianza_explicada) <- nombres_componentes
  componentes_principales <- (X %*% matriz_de_autovectores) %>% as_tibble
  colnames(componentes_principales) <- nombres_componentes
  
  return (list(
    centro_X = attr(X, "scaled:center"), # prcomp()$center
    desvio_X = attr(X, "scaled:scale"), # prcomp()$scale
    Z = componentes_principales, # prcomp()$x
    autovalores = autovalores,
    desvio_Z  = sqrt(autovalores), # prcomp()$sdev
    varianza_total = varianza_total, 
    varianza_explicada = varianza_explicada,
    matriz_de_autovectores = matriz_de_autovectores # prcomp()$rotation
  ))
}

acp <- analisis_de_componentes_principales
