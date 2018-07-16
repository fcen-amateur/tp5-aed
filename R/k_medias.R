library(glue)

k_medias <- function(df, k, max_iteraciones = 50, verboso = F) {
  if (verboso) { print(glue("K-medias con k = {k}")) }
  
  xvars <- colnames(df)
  p <- length(xvars)
  
  # Inicializamos los centroides eligiendo k puntos del dataset al azar
  centroides <-
    sample_n(df, k) %>%
    select(xvars) %>%
    rowid_to_column("centr_id") %>%
    mutate(centr_id = as.character(centr_id))
  
  iteracion <- 0
  los_centroides_se_movieron <- T
  
  while (los_centroides_se_movieron && iteracion < max_iteraciones) {
    iteracion <- iteracion + 1
    
    distancia_a_los_centroides <- function(dato) {
      dato_repetido <- matrix(dato, ncol = p, nrow = k, byrow = T)
      distancias_al_cuadrado <- (select(centroides, -centr_id) - dato_repetido)^2 %>% as.matrix()
      normas_2 <- (distancias_al_cuadrado %*% rep(1, p))^(1/2) %>% c
      distancias <- normas_2 %>% as_tibble %>% mutate(centr_id = centroides$centr_id)
      return (distancias)
    }
  
    elegir_centroide_mas_cercano <- function(distancias) {
      centroide_mas_cercano <- distancias %>% arrange(value) %>% head(1)
      return (centroide_mas_cercano$centr_id)
    }
    
    df_categorizado <-
      df %>%
      mutate(
        distancias_a_centroides  =  pmap(., ~distancia_a_los_centroides(c(...))),
        centr_id  =  map_chr(distancias_a_centroides, elegir_centroide_mas_cercano)
      ) %>%
      select(-distancias_a_centroides)
    
    centroides_recalculados <-
      df_categorizado %>%
      group_by(centr_id) %>%
      select(xvars, centr_id) %>%
      summarise_all(mean)
    
    # Chequear si los centroides se movieron
    distancias_al_cuadrado <- (select(centroides, xvars) - 
                               select(centroides_recalculados, xvars))^2
    distancia_total <- sqrt(sum(distancias_al_cuadrado)) %>% round(8)
    los_centroides_se_movieron <- !near(distancia_total, 0)
    if (verboso) {
      msj = "(Ronda {iteracion}) Los centroides se movieron {distancia_total}"
      print(glue(msj))
    }

    if (los_centroides_se_movieron) { 
      centroides <- centroides_recalculados
    }
  }
  
  return (df_categorizado %>% rename(cluster_id = centr_id))
}
