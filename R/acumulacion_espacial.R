
preparar_para_acumular <- function(X) {
# Este algoritmo espera una tabla que contiene las columnas x e y con las coordenadas de los puntos. Es fácilmente adaptable a n dimensiones pero en su versión actual es bidimensional.
  X <- mutate (X, clase = as.integer(0) , ruido = F, evaluado = F)
  inicial <- sample( 1:length(X$x),1  )
  X[inicial,]$clase <- 1
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

