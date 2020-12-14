################################################################################################################
# 2. Estudia la distribución del numero de películas estrenadas por año. Realiza un gráfico de muestre esta
# distribución haciendo los ajustes necesarios (agrupaciones, cambios de escala, transformaciones. . . )
################################################################################################################
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


###############################################################################################################
# 7. Genera una tabla agrupada por película y año del número de valoraciones.
# Representa la tabla gráficamente para de las 10 películas con mayor número de valoraciones .
###############################################################################################################

group_by(scores, MovieID) %>% 
  summarise(Count_votes = n_distinct(UserID)) 

group_by(scores, year(Date)) %>%
  summarise(Count_votes = n_distinct(UserID)) 

table7 <- group_by(scores, MovieID) %>% 
  group_by(Rated_Year=year(Date), add=T) %>% #Add true so that group_by do not overwrite the previous group_by
  summarise(Count_votes = n_distinct(UserID)) #count users es lo mismo que decir cuantas veces a sido votada.
#lo mismo

table7 <- group_by(scores, MovieID, Year=year(Date)) %>%
  summarise(votes = n_distinct((UserID)))

heatdata <- as.data.frame(spread(table7, key=Year, value=votes, fill=0))
heatdata$total <- rowSums(heatdata[2:8])
sorted <- arrange(heatdata, desc(total))
row.names(sorted) <- sorted$MovieID #A una tibble no le puedes especificar nombres a las filas pero a un dataframe si
heat_m <- as.matrix(select(sorted, -MovieID))
heatmap(heat_m[,-8], Colv=NA,Rowv=NA,col=rainbow(20))

heatmap(head(heat_m[10:1,-8],10), #pelis ordenadas descencientemente 
        cexRow=1.5, cexCol=1.5,
        Colv=NA, Rowv=NA,
        col=rainbow(25))
legend(x="left", legend=c(1:20),
       fill=rainbow(20))

heatmap(head(heat_m[10:1,-8],10), #pelis ordenadas descencientemente 
        cexRow=1.5, cexCol=1.5,
        Colv=NA, Rowv=NA,
        sideColors=c("darkgreen", "yellowgreen"), col=terrain.colors(12))
#añadir barra de colores.
