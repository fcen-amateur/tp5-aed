library("tidyverse")

X <- readRDS("data/GIF/planeta0.rds")
contador <- 0
i <- 0

while (contador <= 999) { 
  X_entrante <- readRDS( paste0("data/GIF/planeta",as.character(contador),".rds") )
  contador <- contador + 1
  if ( !all(X_entrante$clase == X$clase) ) {
    i <- i+1
    X <- X_entrante
    X %>% ggplot( ) +
      aes(x=x,y=y) +
      geom_point( aes(color=as.factor(clase)) ) +
      scale_color_brewer(palette = "Set2", name="Clase") +
      ggtitle("Evolución de la Acumulación") +
      
    ggsave( paste0("imgs/GIF/planeta",str_pad(i,4,"left","0"),".png") )
  }

}
