#1. Puntuaciones por fecha
#2. Puntuaciones por película
#3. Puntuaciones por usuario
#4. Número de puntuaciones por película, usuario y año lanzamiento
#5. Distribucion de los scores (boxplot,barplot)
#6. Series temporales de puntuaciones
#7. Distribución de cuantos usuarios evaluan cuantas pelis totales y diferentes

#BOX PLOT
#plot(x = scores$Score, y = scores$Release_Year)

##Puntuaciones películas cuya publicación es a partir del 2000 (inclido)
prueba1 = scores %>% 
  filter(Release_Year >= 2000)

plot(x = prueba1$Score, y = prueba1$Release_Year)

#De las cinco películas con más número total de valoraciones, 
#compara sus estadísticos y distribuciones
#(histogramas, boxplot, violin plot,. . . )
##boxplot
sample = head(movie_scores[order(movie_scores$n, decreasing = TRUE),],5)

sample
#Mean_Score
#El comando summary nos muestra el valor de la media, la mediana, el mínimo, y
#el máximo de nuestro conjunto de datos y, además,
#nos da información del primer y tercer cuartil (esto lo veremos más adelante).
summary(sample[,"Mean_Score"])
z <- sample[,"Mean_Score"]
#Vamos a representar los estadísticos de la variable `Mean_Score` en un boxplot:
#par(mfrow = c(2,1)) #Divide la ventana de gráficos en 2f y 2c
#boxplot(x = sample$Mean_Score, horizontal = TRUE)
boxplot(z, horizontal = TRUE, col = 123, border = 1)
#par(mfrow = c(1,1)) #Regresa a su estado original
hist(x = sample$MovieID,xlim = c(3.1, 3.99), border = 1, col = 123,
     xlab= 'Mean Score', main = "Histograma de la variable Mean Score")

#SOLUCION#
#Se observa que, la mayoria de los valores se encuentran entre el 3.2 y 3.4, lejos de la mediana.
#En este caso, los valores extremos 3.358 y 3.973, corresponden con a los valores de menos densidad (frecuencia inferior)
#en el histograma

boxplot(sample$Release_Year ~ sample$Mode_Score)

####


##Oye, las 5 películas con mayor numero de puntuación son las siguientes:
top5 = scores %>% 
  filter(MovieID %in% c('6037', '8387' , '10730' , '313', '9645'))

#Boxplot del score de las 5 películas
View(top5)
boxplot(top5$Score, horizontal = TRUE, col = 123, border = 1)



#Histograma del Release_Year de las 5 películas
hist(x = top5$Score,xlim = c(3.5, 3.99), border = 1, col = 123,
     xlab= 'Mean Score', main = "Histograma de la variable Mean Score")
#Se observa que la mayoría de las puntuaciones de estas cinco películas se encuentran en el rango 3.8 y 4.
#El Boxplot nos indica que la mediana es 4. Y vemos un valores aislados de puntuaciones muy bajas.
summary(top5$Score)


#Box
boxplot(top5$Score ~ top5$Release_Year, main)
boxplot(top5$Score ~ top5$MovieID)

library(ggplot2)
##Oye, las 5 películas con mayor numero de puntuación son las siguientes:
top5 = scores %>% 
  filter(MovieID %in% c('6037', '8387' , '10730' , '313', '9645'))

#Boxplot del score de las 5 películas
View(top5)
boxplot(top5$Score, horizontal = TRUE, col = 123, border = 1)
###Boxplot donde se relaciona el score con el año de estreno de la película
pl <- ggplot(top5, aes(x = Score, y = Release_Year, fill = factor(MovieID)))
pl + geom_boxplot() + theme_bw() + coord_flip() + labs(fill = "Year") +
  scale_y_continuous(breaks = seq(1999, 2005, 1))


##Histogram
# Basic histogram
ggplot(top5, aes(x=top5$Score))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5) + 
  geom_histogram(bins=5) +
  labs(title="Histograma puntuaciones top 5 películas",x="Scores", y = "Densidad")+
  theme_bw()

ggplot(data = top5, aes(x = Score, y = Release_Year)) + 
  geom_point(color = 'red', fill = 'red', size = 4, shape = 18, alpha = 0.5) +
  #geom_smooth(color = 'red') + #para poner línea de tendencia
  xlab('Puntuación Media') + 
  ylab('Release_Year') +
  ggtitle('Relación entre la puntuación  y el año de estreno') + 
  theme_test()
