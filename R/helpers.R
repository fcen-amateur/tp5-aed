ratio_a_porcentaje <- function(ratio) {
  porcentaje <- round(ratio * 100, 2)
  return (paste(porcentaje, "%", sep = ""))
}
