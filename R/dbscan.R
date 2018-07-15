dbscan <- function(X, epsilon = 0.5, min_pts = 5, fun_dist = distancia_euclidea) {
  
  if (is.data.frame(X)) { X <- data.matrix(X) }
  
  N <- dim(X)[1]
  
  etiquetas <- rep(0, N)
 
  clase <- 0
  
  for (punto in seq(N)) {
    # si el punto ya fue etiquetado, seguir
    if (etiquetas[[punto]] != 0) { next }
    
    # computar los vecinos directos del punto en cuestión
    vecinos_directos <- vecinos(X, punto, epsilon, fun_dist)
    
    # si tiene menos de min_pts vecinos, etiquetar como ruido y continuar
    if (length(vecinos_directos) < min_pts) {
      etiquetas[[punto]] <- -1
      next
    }
    
    # si no, etiquetar al punto con una nueva clase
    clase <- clase + 1
    etiquetas[[punto]] <- clase

    # inicializo la list de vecinos con los vecinos directos    
    vecinos <- vecinos_directos
    # mientras haya algun vecino del punto sin etiquetar, explorar su vecindad
    while (any(etiquetas[vecinos] == 0)) {
      for (punto_vecino in vecinos) {
        # cambiar las etiquetas de ruido por la de la clase actual
        if (etiquetas[[punto_vecino]] == -1) { etiquetas[[punto_vecino]] <- clase }
        # ignorar los puntos ya etiquetados
        if (etiquetas[[punto_vecino]] != 0) { next }
        # si encuentro un punto sin etiquetar, etiquetarlo y buscar sus vecinos ("vecinos segundos" del punto original)
        etiquetas[[punto_vecino]] <- clase
        vecinos_segundos <- vecinos(X, punto_vecino, epsilon, fun_dist)
        # si el punto vecino tiene suficientes "vecinos segundos", incorporarlos a la vecindad del punto original
        if (length(vecinos_segundos) >= min_pts) { 
          vecinos <- unique(c(vecinos, vecinos_segundos))
        }
      }
    }
  }
  return(etiquetas)
}

vecinos <- function(X, i, epsilon, fun_dist) {
  # devuelve un vector con los índices de los puntos vecinos al i-ésimo punto en df
  distancias <- apply(X, 1, function(x){fun_dist(X[i,], x)})
  which(distancias < epsilon)
}

distancia_euclidea <- function(x, y) {
  # Comento los pasos de seguridad porque ralentizan >10x la ejecucion
  # assertthat::assert_that(length(x) == length(y))
  # assertthat::assert_that(is.numeric(x) && is.numeric(y))
  sqrt(sum((x - y)^2))
}


# DBSCAN(DB, distFunc, eps, minPts) {
#    C = 0                                                  /* Cluster counter */
#    for each point P in database DB {
#       if label(P) ≠ undefined then continue               /* Previously processed in inner loop */
#       Neighbors N = RangeQuery(DB, distFunc, P, eps)      /* Find neighbors */
#       if |N| < minPts then {                              /* Density check */
#          label(P) = Noise                                 /* Label as Noise */
#          continue
#       }
#       C = C + 1                                           /* next cluster label */
#       label(P) = C                                        /* Label initial point */
#       Seed set S = N \ {P}                                /* Neighbors to expand */
#       for each point Q in S {                             /* Process every seed point */
#          if label(Q) = Noise then label(Q) = C            /* Change Noise to border point */
#          if label(Q) ≠ undefined then continue            /* Previously processed */
#          label(Q) = C                                     /* Label neighbor */
#          Neighbors N = RangeQuery(DB, distFunc, Q, eps)   /* Find neighbors */
#          if |N| ≥ minPts then {                           /* Density check */
#             S = S ∪ N                                     /* Add new neighbors to seed set */
#          }
#       }
#    }
# }
# 
# RangeQuery(DB, distFunc, Q, eps) {
#    Neighbors = empty list
#    for each point P in database DB {                      /* Scan all points in the database */
#       if distFunc(Q, P) ≤ eps then {                      /* Compute distance and check epsilon */
#          Neighbors = Neighbors ∪ {P}                      /* Add to result */
#       }
#    }
#    return Neighbors
# }