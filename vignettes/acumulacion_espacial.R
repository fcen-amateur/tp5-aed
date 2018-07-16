library("tidyverse")

set.seed(42)

planeta_chico <- readRDS("data/planeta_y_anillo.rds")
planeta_y_anillo <- readRDS("data/planeta_y_anillo1.rds")

X_ac <- readRDS("data/vignette_planeta_grande.rds")
Y_ac <- readRDS("data/vignette_planeta_chico.rds")
U_ac <- readRDS("data/vignette_planeta_grande_dbscan.rds")
V_ac <- readRDS("data/vignette_planeta_chico_dbscan.rds")

#Para que sea más fácil iterar el algoritmo principal, las tareas de arranque las hace este: Añade la columna de clase y una que indica si el punto es ruido. Esta va a servir para poder ir restando los puntos ya evaluados y que no parezca que los remanentes son ruido cuando en realidad ya son parte de una clase que fue borrada. Además, elige el primer integrante de la primera clase.

preparar_para_acumular <- function(X) {
# Este algoritmo espera una tabla que contiene las columnas x e y con las coordenadas de los puntos. Es fácilmente adaptable a n dimensiones pero en su versión actual es bidimensional.
  X <- mutate (X, clase = as.integer(0) , ruido = F, evaluado = F)
  inicial <- sample( 1:length(X$x),1  )
  X[inicial,]$clase <- 1
  return(X)
}

#Cuando evaluemos una fila de la matriz, vamos a ahorrarnos el cálculo de la distancia si la fila ya fue clasificada usando esta función. 

medir_si_no_tiene_clase <- function(x,y,x_p,y_p,eps,clase) {
  if (clase == 0) {
  return ( (x-x_p)^2 + (y-y_p)^2 <= eps )
  }
  else { return( F  ) }
}

#Estas funciones esquematizan las etapas del algoritmo, hallar un punto no clasificado, evaluar quiénes entre los puntos aún no clasificados son sus vecinos y decidir si el punto es ruido. Esto último se logra considerando que la única diferencia fundamental es que los puntos ruidosos no tienen suficientes vecinos para iniciar una clase, entonces al empezar una clase llamamos una función especial. 

buscar_p <- function(X,clase_actual) {
  for (i in  1:nrow(X) ) {
    vec <- X[i,]
    if (!vec$evaluado && vec$clase==clase_actual) { return(list(indice=i, clase=clase_actual, clase_nueva=F, termina=F)) }
  }
  for (i in  1:nrow(X) ) {
    vec <- X[i,]
    if (!vec$evaluado && vec$clase == 0) { return( list(indice=i,clase=clase_actual+1,clase_nueva=T, termina = F)  )}
    }
  if ( prod(X$evaluado) == 1 ) { return( list(indice=-1,clase=-1,clase_nueva=F,termina=T) )  }
}

vecinos_p_clase_nueva <- function( X, indice, clase_actual, eps, umbral ) {
  x_p <- X[indice, ]$x
  y_p <- X[indice, ]$y
  X[indice, ]$evaluado <- T
  X %>% rowwise()  %>% mutate(clase =
	               ifelse( 
			      medir_si_no_tiene_clase(x,y,x_p,y_p,eps,clase) ,
			      clase_actual ,
			      clase)
	       ) -> X_tentativa
  vecinos_nuevos <- length(which( X_tentativa$clase == clase_actual )) - length( which( X$clase == clase_actual ))
  X[indice, ]$ruido <- T
  X_salida <- if ( vecinos_nuevos >= umbral ) {X_tentativa} else {X} 
  return(X_salida)
}

vecinos_p <- function( X, indice, clase_actual, eps, umbral ) {
  x_p <- X[indice, ]$x
  y_p <- X[indice, ]$y
  X[indice, ]$evaluado <- T
  X %>% rowwise() %>% mutate(clase =
	               ifelse( 
			      medir_si_no_tiene_clase(x,y,x_p,y_p,eps,clase) ,
			      clase_actual ,
			      clase)
	       ) -> X_tentativa
  return(X_tentativa)
}

acumulacion <- function( X, eps, umbral, clase_actual=1,contador=0 ) {
#Este algoritmo organiza el proceso, la idea es que sea recursivo. Espera una tabla preparada previamente con el algoritmo "preparar para acumular".
  busqueda <- buscar_p(X,clase_actual)
  if (busqueda$termina==T) {return (X)}
  indice_punto <- busqueda$indice 
  clase_actual <- busqueda$clase
  if ( busqueda$clase_nueva ) {
	    X <-  vecinos_p_clase_nueva(X, indice_punto, clase_actual, eps, umbral ) 
       }
       else {
	    X <-  vecinos_p(X, indice_punto, clase_actual, eps, umbral )
       }
  return(
	 acumulacion(X,eps,umbral,clase_actual,contador+1)
	 )
}

set.seed(42)

#X <- preparar_para_acumular( planeta_y_anillo )
#X_ac <- acumulacion(X,0.4,2)

#saveRDS(X_ac,"data/vignette_planeta_grande.rds")

Y <- preparar_para_acumular( planeta_chico )
Y_ac <- acumulacion(Y,0.8,4)

saveRDS(Y_ac,"data/vignette_planeta_chico.rds")

graficar <-  function(X,etiqueta=" ") {
  ggplot(data=X) +
    aes(x=x,y=y,color=as.factor(clase) ) +
    geom_point() +
    scale_color_discrete( name="Clase" ) +
    ggtitle(etiqueta)
}

graficar_con_ruido <-  function(X,etiqueta=" ") {
  ggplot(data=X) +
    aes(x=x,y=y,color=as.factor(clase),shape=ruido ) +
    geom_point() +
    scale_color_discrete( name="Clase" ) +
    scale_shape_manual(values=c(16,1)) +
    ggtitle(etiqueta)
}

# Como se observa gráficamente, la baja cantidad de puntos no es un problema, se identifica correctamente los conjuntos aún con 200 puntos. 

source("R/dbscan.R")

#U_ac <- planeta_y_anillo %>%
#  mutate(clase = dbscan(planeta_y_anillo[c("x", "y")], min_pts = 2, epsilon = .4))

#saveRDS(U_ac,"data/vignette_planeta_grande_dbscan.rds")

#V_ac <- planeta_chico %>%
#  mutate(clase = dbscan(planeta_chico[c("x", "y")], min_pts = 4, epsilon = .8))

#saveRDS(U_ac,"data/vignette_planeta_chico_dbscan.rds")



