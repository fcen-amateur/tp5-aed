set.seed(42)

interpretar_coordenada_x <- function (radio, argumento) {
  return( radio * cos(argumento) )
}

interpretar_coordenada_y <- function (radio, argumento) {
  return( radio * sin(argumento) )
}

dibujar_planeta <- function(puntos) {
  radios_planeta <- runif( n = puntos, min = 0, max= 4 )
  angulos_planeta <- runif( n = puntos, min = 0, 2*pi )

  x_planeta <- map2_dbl(radios_planeta, angulos_planeta, interpretar_coordenada_x)
  y_planeta <- map2_dbl(radios_planeta, angulos_planeta, interpretar_coordenada_y)

  radios_anillo <- runif( n = puntos, min = 5, max= 5.5 )
  angulos_anillo <- runif( n = puntos, min = 0, 2*pi )

  x_anillo <- map2_dbl(radios_anillo, angulos_anillo, interpretar_coordenada_x)
  y_anillo <- map2_dbl(radios_anillo, angulos_anillo, interpretar_coordenada_y)

  x <- append(x_planeta,x_anillo)
  y <- append(y_planeta,y_anillo)

#Para no hacersela más fácil al algoritmo, vamos a unir las listas usando sample:

  rifa <- sample( 1:length(x), replace=F ) 

  planeta_y_anillo <- tibble("x" = x[c(rifa)],"y"= y[c(rifa)] )

  saveRDS(planeta_y_anillo,"data/planeta_y_anillo1.rds")

  planeta_y_anillo %>% ggplot() +
    geom_point() +
    aes(x=x,y=y)
  ggsave("imgs/planeta1.png")
}

