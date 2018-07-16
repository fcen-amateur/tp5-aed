source("R/funcion_dibujar_planeta")
set.seed(42)

planeta_y_anillo <- dibujar_planeta(200) 

saveRDS(planeta_y_anillo,"data/planeta_y_anillo.rds")

planeta_y_anillo %>% ggplot() +
	geom_point() +
	aes(x=x,y=y)
ggsave("imgs/planeta.png")


planeta_y_anillo <- dibujar_planeta(500) 

saveRDS(planeta_y_anillo,"data/planeta_y_anillo1.rds")

planeta_y_anillo %>% ggplot() +
	geom_point() +
	aes(x=x,y=y)
ggsave("imgs/planeta1000.png")
