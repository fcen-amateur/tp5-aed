# El proceso de etiquetado requiere constantemente tomar distancias, respcto a los puntos no etiquetados. Para no tomar distancias innecesarias, es importante mantener algunas convenciones. 
# Los numeros de observación sirven para darle una identidad única a las distancias y no repetir d(A,B) para hacer d(B,A). El algoritmo nunca debe trabajar con los puntos ya etiquetados.
# El primero paso, entonces, es introducir una columna de etiquetas "clase" y etiquetar todos los puntos en la clase 0. Siempre vamos a condicionar el trabajo sobre nuevos puntos a que esta sea su clase.


