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

#VIOLIN PLOT
library(ggplot2)
View(top5)
# Convert the variable dose from a numeric to a factor variable
#top5$Score <- as.integer(top5$Score)
#top5$MovieID <- as.integer(top5$MovieID)
#head(top5)

# Puntuación de los usuarios según la película
#top5$MovieID <- as.integer(top5$MovieID)
top5$MovieID <- as.character(top5$MovieID)
he_plot =ggplot(data = top5, aes(x = MovieID, y = Score)) + 
  geom_point(aes(color = MovieID), size = 1, alpha = 0.7) +
  xlab('Película') + 
  ylab('Puntuación dada por el usuario') +
  ggtitle('Puntuación de los usuarios según la película') + 
  theme_minimal()
he_plot


plot2 = ggplot(data = top5, aes(x = MovieID, y = Score)) + 
  #geom_jitter(size = 1, color = 'gray', alpha = 0.5) + 
  geom_violin(aes(fill = MovieID), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Género') + 
  ylab('Identificador Película') +
  ggtitle('Puntuación de los usuarios según la película') + 
  theme_bw()
plot2

#######################################################EJERCICIO 9########################################
#########################################################################################################

#1. Puntuaciones por fecha
#2. Puntuaciones por película
#3. Puntuaciones por usuario
#4. Número de puntuaciones por película, usuario y año lanzamiento
#5. Distribucion de los scores (boxplot,barplot)
#6. Series temporales de puntuaciones
#7. Distribución de cuantos usuarios evaluan cuantas pelis totales y diferentes


##Primero me creo una tabla seleccionando los campos que quiero comparar
cols <- c(4,13)

newtab=scores[,cols]
newtab
#scores
x = table(newtab)#frecuencias absolutas
prop.table(x) #frecuencias relativas
addmargins(x) #añadir los totales a la tabla de frecuencias absolutas

###en qué mes se hubo mas votaciones
barplot(table(newtab$Month), #col=c("lightblue","pink"),
        xlab="Meses", ylab="Frecuencia", 
        main="test")

###más detalle puntuaciones por trimestres
###FILTRAR
#filter(combined_data_1, Score == 5 | Score == 4)#Usuarios que han puntuado la película con un Score igual a `5` o `4`
#Score_4_5 <- filter(combined_data_1, Score %in% c(4,5)) #otra manera de obtener lo de arriba

##Aquí tenemos el 1Q separado:
Q1 = filter(newtab, Month == 'Jan' |Month == 'Feb' | Month == 'Mar' | Month == 'Apr')
Q2 = filter(newtab, Month == 'May' | Month == 'Jun'| Month == 'Jul' | Month == 'Aug')
Q3 = filter(newtab, Month == 'Sep' |Month == 'Oct' | Month == 'Nov' | Month == 'Dec')

###solución: barplot por Q (sin etiquetado)

QI<-ggplot(data=Q1, aes(x=Month, y=Score, fill = Month)) +
  geom_bar(stat="identity") +
  scale_y_continuous(limit = c(0,500000,1000000,1500000, 2000000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  theme_bw()
QI

QII<-ggplot(data=Q2, aes(x=Month, y=Score, fill = Month)) +
  geom_bar(stat="identity") +
  scale_y_continuous(limit = c(0,500000,1000000,1500000, 2000000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  theme_bw()
QII

QIII<-ggplot(data=Q3, aes(x=Month, y=Score, fill = Month)) +
  geom_bar(stat="identity") +
  scale_y_continuous(limit = c(0,500000,1000000,1500000, 2000000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  theme_bw()
QIII

## GRÁFICOS CON Etiquetas
Q1
q1 <- aggregate(Q1$Score, by=list(Monthh=Q1$Month), FUN = sum)
q1 <- as.data.frame(q1)
names(q1) <- c("Month", "Score")
gg <- ggplot(q1, aes(x = Month, y = Score, fill = Month)) + 
  geom_col() +
  geom_text(aes(label = Score), vjust = -0.5) +
  scale_y_continuous(limit = c(0,500000,1000000,1500000, 2000000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  ggtitle ("Puntuaciones totales del 1Q") +
  theme_bw()
print(gg)

q2 <- aggregate(Q2$Score, by=list(Monthh=Q2$Month), FUN = sum)
q2 <- as.data.frame(q2)
names(q2) <- c("Month", "Score")
gg2 <- ggplot(q2, aes(x = Month, y = Score, fill = Month)) + 
  geom_col() +
  geom_text(aes(label = Score), vjust = -0.5)+
  scale_y_continuous(limit = c(0,600000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  ggtitle ("Puntuaciones totales del 1Q") +
  theme_bw()
print(gg2)

q3 <- aggregate(Q3$Score, by=list(Monthh=Q3$Month), FUN = sum)
q3 <- as.data.frame(q3)
names(q3) <- c("Month", "Score")
gg3 <- ggplot(q3, aes(x = Month, y = Score, fill = Month)) + 
  geom_col() +
  geom_text(aes(label = Score), vjust = -0.5)+
  scale_y_continuous(limit = c(0,600000)) +
  geom_bar(stat='identity') + geom_point() + expand_limits(x=0,y=0) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=c("#003057", "#B52555", "#A0D1CA", "#7A99AC" )) +
  ggtitle ("Puntuaciones totales del 1Q") +
  theme_bw()
print(gg3)


##Las películas más votadas por mes
glimpse(scores)
Jan = filter(newtab, Month=='Jan')
Feb = filter(newtab, Month=='Feb')
Mar = filter(newtab, Month=='Mar')
Apr = filter(newtab, Month=='Apr')
May = filter(newtab, Month=='May')
Jun = filter(newtab, Month=='Jun')
Jul = filter(newtab, Month=='Jul')
Aug = filter(newtab, Month=='Aug')
Sep = filter(newtab, Month=='Sep')
Oct = filter(newtab, Month=='Oct')
Nov = filter(newtab, Month=='Nov')
Dec = filter(newtab, Month=='Dec')

##En enero cual fue la pelicula más votada:
test <- Dec %>%
  group_by(MovieID) %>%
  summarise(Sum_Score = sum(Score), Mean_Score = mean(Score), SD_Score = sd(Score), Mode_Score = mlv(Score), Median_Score = median(Score) , n = n()) %>%
  left_join(titles, by = 'MovieID')

kable(head(test %>% arrange(desc(Mean_Score))))

kable(head(test %>% arrange(desc(n))))

# >, >=, <, <=, ==, !=




remove(q1)
remove(q2)
remove(q3)


















