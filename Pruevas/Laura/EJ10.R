#Ejercicio 8
#◘8. Distribución del score promedio por año de las 10 películas con mayor número de valoraciones.
library(ggplot2)
#Elegir el TOP 10 películas con mayor número de valoraciones:
ej_10 = head(movie_scores[order(movie_scores$n, decreasing = TRUE),],10)
ej_10

# Representarlo en un geom_point
p <- ggplot(data = ej_10, aes(x = Release_Year, y = Mean_Score)) + 
  geom_point(color = 'red', fill = 'red', size = 4, shape = 18, alpha = 0.5) +
  #geom_smooth(color = 'red') + #para poner línea de tendencia
  ylab('Mean_Score') + 
  xlab('Release_Year') +
  ggtitle('Relation between Mean_Score & Release_Year') + 
  theme_test()

#OPCIÓN 1
p+ geom_rug(outside = TRUE, sides = "tr") +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

#OPCIÓN 2
p + geom_rug(length = unit(0.05, "npc"), color = 'darkmagenta') +
  scale_y_continuous(expand = c(0.1, 0.1))

#OPCIÓN 3 (mi preferida es esta)
ggplot() +
  #geom_point(color = 'red', fill = 'red', size = 4, shape = 18, alpha = 0.5) +
  geom_rug(data=ej_10, mapping=aes(x=Mean_Score), color="darkorange") +
  geom_rug(data=ej_10, mapping=aes(y=Release_Year), color="darkmagenta") +
  geom_point(data=ej_10, mapping=aes(x=Mean_Score, y=Release_Year), color = 'darkturquoise', fill = 'darkmagenta', size = 4, shape = 18, alpha = 0.5) +
  ggtitle("Distribución del score promedio por año de las 10 películas
          con mayor número de valoraciones") +
  #theme(plot.title = element_text(size = 10, face = "bold", family = 'Century')) + 
  xlab('Puntuación media') +
  ylab('Año de estreno') +
  theme_bw() +
  theme(text=element_text(family="Broadway", face="bold", size=10))


#install.packages("extrafont")
library(extrafont)
#font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                       #vector of font family names
         
  
  