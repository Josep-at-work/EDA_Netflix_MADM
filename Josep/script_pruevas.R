# 2. Estudia la distribución del numero de películas estrenadas por año. Realiza un gráfico de muestre esta
# distribución haciendo los ajustes necesarios (agrupaciones, cambios de escala, transformaciones. . . )

scores
id_year <- group_by(scores, MovieID) %>% 
  summarise(Release_Year = unique(Release_Year)) 
movies_per_year <- id_year %>% group_by(Release_Year) %>% 
  summarise(Count_Movies = n_distinct(MovieID))
which(movies_year$Release_Year==2000)


#Nuestras 250 pelis agrupadas por el año en que se publicaron
summary(movies_per_year)
#el año que se estrenaron más películas se estrenaron 25 y
#en un 50% de los años se estrenaron como mucho 2 pelis.
#(tener en cuanta que esto es siempre sobre nuestra 250)

#scatter plot
ggplot(data=movies_per_year) + #sistema de coordenadas al que añadir puntos(creates an empty graph)
  geom_point(mapping=aes(Release_Year, Count_Movies), color='blue', fill='red', shape=21)  #this is a type of layer you can add.(Scatterplot)
#if you can add a third class add the aes color(or size or alpha or shape)=third class

# “The greatest value of a picture is when it forces us to notice what we never expected to see.” — John Tukey

#lineplot con función de densidad

ggplot(data=movies_per_year) + 
  geom_smooth(mapping=aes(Release_Year, Count_Movies)) 

#Barplot

ggplot(data=id_year) + 
  geom_bar(mapping=aes(x=Release_Year), stat='count', color='blue') # no pide y, le metes una variable y ya te contea por defecto
#se puede poner otros stats.

#Mismo grafico
ggplot(data=movies_per_year) + 
  geom_bar(mapping=aes(x=Release_Year, y=Count_Movies), stat='identity') #Identity coje como altura la y

ggplot(data=movies_per_year) + 
  stat_summary(
    mapping= aes(x=Release_Year, y=Count_Movies)
  )

#Combos
ggplot(data=movies_per_year, mapping=aes(Release_Year, Count_Movies)) + 
  geom_smooth() +
  geom_point()

# En estos graficos marcar con una linia los años que se hicieron 2 peliculas o menos o algo así para añadir cositas