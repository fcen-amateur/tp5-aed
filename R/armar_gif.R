library("tidyverse")
library("RColorBrewer")

colores <- brewer.pal(3,"Set2")

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
      geom_point( aes(color=as.factor(clase),shape=as.factor(clase)) ) +
      scale_color_manual(name = "Clase Asignada",
                         values=c("0" = colores[1],
                                  "1" = colores[2],
                                  "2" = colores[3]
#                                  ),
#                         labels=c("0","1","2")) +
                                  )) +
      scale_shape_manual(name="Clasificado",
                         values=c( "0" = 1,
                                   "1" = 16,
                                  "2" = 17),
                         labels=c("0" = "No Clasificado",
                                  "1" = "Planeta",
                                  "2" = "Anillo")
                         ) +
      ggtitle("Evolución de la Acumulación") +
      coord_fixed()
      
    ggsave( paste0("imgs/GIF/planeta",str_pad(i,4,"left","0"),".png"), width=8.5, height=7 )
  }

}
