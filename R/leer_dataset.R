library(tidyverse)

leer_dataset <- function(filename) {
  nombres_castellano <- c(
    "Alcohol" = "alcohol",
    "MalicAcid" = "acido_malico",
    "Ash" = "ceniza",
    "AshAlcalinity" = "alcalinidad_ceniza",
    "Magnesium" = "magnesio",
    "TotalPhenols" = "fenoles_totales",
    "Flavanoids" = "flavonoides",
    "NonflavanoidPhenols" = "no_flavonoides",
    "Proanthocyanins" = "proantocianinas",
    "ColorIntensity" = "intensidad_del_color",
    "Hue" = "tonalidad",
    "OD280_OD315" = "OD280_OD315",
    "Proline" = "prolina"
  )

  read_delim(filename, delim = " ") %>%
    plyr::rename(nombres_castellano) %>%
    rowid_to_column("obs_id")
}
