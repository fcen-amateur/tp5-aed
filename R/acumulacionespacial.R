library("tidyverse")

# El proceso de etiquetado requiere constantemente tomar distancias, respcto a los puntos no etiquetados. Para no tomar distancias innecesarias, es importante mantener algunas convenciones. 
# Los numeros de observación sirven para darle una identidad única a las distancias y no repetir d(A,B) para hacer d(B,A). El algoritmo nunca debe trabajar con los puntos ya etiquetados.
# El primero paso, entonces, es introducir una columna de etiquetas "clase" y etiquetar todos los puntos en la clase 0. Siempre vamos a condicionar el trabajo sobre nuevos puntos a que esta sea su clase.

planeta_y_anillo <- readRDS("../data/planeta_y_anillo.rds")

distancia_a_un_punto <- function ( X, p) {
  n <-  length(X[,1])
  X <- as.matrix(X-p)
  return (  X^2  %*% c(1,1) )  
}


#Para que sea más fácil iterar el algoritmo principal, las tareas de arranque las hace este: Añade la columna de clase y una que indica si el punto es ruido. Esta va a servir para poder ir restando los puntos ya evaluados y que no parezca que los remanentes son ruido cuando en realidad ya son parte de una clase que fue borrada. Además, elige el primer integrante de la primera clase.

preparar_para_acumular <- function(X) {
  X <- mutate ( X, clase = rep(NaN) , ruido = rep(NaN))
  inicial <- sample( 1:length(X$x),1  )
  X[inicial,]$clase <- 1
  return(X)
}

acumulacion <- function( X, eps, umbral, ultimaclase ) {
#Este algoritmo organiza el proceso, la idea es que sea iterativo. Espera una tabla preparada previamente con el algoritmo "preparar para acumular".
}
