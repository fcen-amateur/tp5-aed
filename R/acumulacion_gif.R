library("tidyverse")

set.seed(42)

# El proceso de etiquetado requiere constantemente tomar distancias, respcto a los puntos no etiquetados. Para no tomar distancias innecesarias, es importante mantener algunas convenciones. 
# Los numeros de observación sirven para darle una identidad única a las distancias y no repetir d(A,B) para hacer d(B,A). El algoritmo nunca debe trabajar con los puntos ya etiquetados.
# El primero paso, entonces, es introducir una columna de etiquetas "clase" y etiquetar todos los puntos en la clase 0. Siempre vamos a condicionar el trabajo sobre nuevos puntos a que esta sea su clase.

planeta_y_anillo <- readRDS("data/planeta_y_anillo1.rds")

#distancia_a_un_punto <- function ( X, ind) {
#  X <- X[,c(1,2)]
#  p <- as.list(X[ind,])
#  X <- as.matrix(X-p)
#  return (  X^2  %*% c(1,1) )  
#}


#Para que sea más fácil iterar el algoritmo principal, las tareas de arranque las hace este: Añade la columna de clase y una que indica si el punto es ruido. Esta va a servir para poder ir restando los puntos ya evaluados y que no parezca que los remanentes son ruido cuando en realidad ya son parte de una clase que fue borrada. Además, elige el primer integrante de la primera clase.

preparar_para_acumular <- function(X) {
# Este algoritmo espera una tabla que contiene las columnas x e y con las coordenadas de los puntos. Es fácilmente adaptable a n dimensiones pero en su versión actual es bidimensional.
  X <- mutate (X, clase = as.integer(0) , ruido = F, evaluado = F)
  inicial <- sample( 1:length(X$x),1  )
  X[inicial,]$clase <- as.integer(1)
  return(X)
}

medir_si_no_tiene_clase <- function(x,y,x_p,y_p,eps,clase) {
  if (clase == 0) {
  return ( (x-x_p)^2 + (y-y_p)^2 <= eps )
  }
  else { return( F  ) }
}

buscar_p <- function(X,clase_actual) {
  for (i in  1:nrow(X) ) {
    vec <- X[i,]
    if (!vec$evaluado && vec$clase==clase_actual) { return(list(indice=i, clase=clase_actual, clase_nueva=F, termina=F)) }
  }
  for (i in  1:nrow(X) ) {
    vec <- X[i,]
    if (!vec$evaluado && vec$clase == 0) { return( list(indice=i,clase=clase_actual + as.integer(1),clase_nueva=T, termina = F)  )}
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

acumulacion <- function( X, eps, umbral, clase_actual=as.integer(1),contador=as.integer(0) ) {
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
  saveRDS( X, paste0("data/GIF/","planeta",as.character( contador ),".rds") )
  return(
	 acumulacion(X,eps,umbral,clase_actual,contador+ as.integer(1) )
	 )

}

set.seed(42)

X <- preparar_para_acumular( planeta_y_anillo )
X_ac <- acumulacion(X,0.8,2)
