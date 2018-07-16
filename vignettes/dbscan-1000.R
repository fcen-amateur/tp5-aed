library(tidyverse)

planeta <- readRDS("data/planeta_y_anillo1.rds")

source("R/dbscan.R")

set.seed(42)

planeta <- planeta %>%
  mutate(clase = dbscan(planeta[c("x", "y")], min_pts = 4, epsilon = .8))

planeta %>%
  ggplot(aes(x,y, color = factor(clase))) +
  geom_point()

vecinos(planeta, 3, epsilon = 1, distancia_euclidea)
