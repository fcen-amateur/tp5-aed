library(tidyverse)

leer_dataset <- function(dataset) {
  if (dataset == "vino" ||
      dataset == "data/wine.txt") {
    tipos_columnas <- cols(
      Alcohol = col_double(),
      MalicAcid = col_double(),
      Ash = col_double(),
      AshAlcalinity = col_double(),
      Magnesium = col_integer(),
      TotalPhenols = col_double(),
      Flavanoids = col_double(),
      NonflavanoidPhenols = col_double(),
      Proanthocyanins = col_double(),
      ColorIntensity = col_double(),
      Hue = col_double(),
      OD280_OD315 = col_double(),
      Proline = col_integer()
    )
    
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
  
    df <- read_delim(
      "data/wine.txt", delim = " ", col_types = tipos_columnas) %>%
      plyr::rename(nombres_castellano) %>%
      rowid_to_column("obs_id")
  } else if (
    dataset == "planeta_y_anillo" ||
    dataset == "data/planeta_y_anillo.rds") {
    df <- read_rds("data/planeta_y_anillo.rds")
  } else {
    # Si no reconoce `dataset`, intenta leer un CSV como tibble
    df <- read_csv(dataset)
  }

  return(df)
}
