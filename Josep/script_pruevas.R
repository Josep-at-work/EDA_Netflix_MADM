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

##################scatter plot

ggplot(data=movies_per_year) + #sistema de coordenadas al que añadir puntos(creates an empty graph)
  geom_point(mapping=aes(Release_Year, Count_Movies),color='blue', fill='red', shape=21)  #this is a type of layer you can add.(Scatterplot)
  #if you can add a third class add the aes color(or size or alpha or shape)=third class

# “The greatest value of a picture is when it forces us to notice what we never expected to see.” — John Tukey

###################Lineplot con función de densidad

ggplot(data=movies_per_year) + 
  geom_smooth(mapping=aes(Release_Year, Count_Movies)) 

###################Density
library(hrbrthemes)
ggplot(data=id_year, aes(x=Release_Year)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_ipsum()
  

###################Barplot

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

################## run from here
table7 <- group_by(scores, MovieID, Year=year(Date)) %>%
  summarise(votes = n_distinct((UserID)))

heatdata <- as.data.frame(spread(table7, key=Year, value=votes, fill=0))
heatdata$total <- rowSums(heatdata[2:8])
sorted <- arrange(heatdata, desc(total))
row.names(sorted) <- sorted$MovieID #A una tibble no le puedes especificar nombres a las filas pero a un dataframe si
heat_m <- as.matrix(select(sorted, -MovieID))
################# run till here

################# heatmaps

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



ggplot(head(sorted[10:1,-c(1,8)],10), aes(x = Year, y = votes, fill = MovieID)) + geom_tile()

mat.heatmap = matrix(runif(100), ncol = 10, nrow = 10)
colnames(mat.heatmap) = 1:10
rownames(mat.heatmap) = letters[1:10]

head(mat.heatmap)
require(reshape)
mat.heatmap.m = melt(mat.heatmap)
head(mat.heatmap.m)

ggplot(data = data.frame(mat.heatmap.m), aes(x = X1, y = X2, fill= value)) + geom_tile()

df.heatmap <- expand.grid(Var1 = letters[1:10], Var2 = 1:10)
df.heatmap$score <- runif(nrow(df.heatmap), min = -2, max = 2)

############################################################################ form here last version

table7.1 <- group_by(scores, MovieID, Year=year(Date)) %>%
  summarise(votes = n_distinct((UserID)))

table7.2 <- group_by(scores, MovieID) %>%
  summarise(votes = n_distinct((UserID)))

top10 <- head(arrange(table7.2, desc(votes)), 10)

movies_onfire <- filter(table7.1, MovieID %in% top10$MovieID) #onfire porque es un heatmap y son las pelis mas votadas(chiste)

# times_voted <- group_by(movies_onfire, MovieID) %>%
#   summarise(count = n_distinct(Year))
# #ordenar con el ranking
# t_v <- times_voted[match(top10$MovieID, times_voted$MovieID),]

######################### specific order
for (i in 1:10) {
  movie <- top10$MovieID[i]
  indexes <- which(movies_onfire$MovieID == movie)
  movies_onfire$MovieID <- replace(movies_onfire$MovieID, indexes, i)
  count=i
}

########################################## Heatmap

barplot(height = movies_onfire$votes, ylim =c(1,80000))#Cambiando los limites de y que intervalos son significativos

secuencia <- cut(movies_onfire$votes,
                 breaks = c(min(movies_onfire$votes), 1000, 2000, 3000, 5000, 7000, 10000, 15000,
                            20000, 25000, 30000,35000,40000,45000,50000,max(movies_onfire$votes)),
                 labels=c('0<','1k-2k', '2k-3k', '3k-5k', '5k-7k', '7k-10k', '10k-15k', '15k-20k',
                          '20k-25k', '25k-30k', '30k-35k', '35k-40k', '40k-45k', '45k-50k','>85k'),
                 include.lowest = T) #15 values

####################### Paleta de 15 colores
library(RColorBrewer)
nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(nb.cols)

####################### ggplot y ggplotly
library(ggplot2)
ggplot(movies_onfire, aes(text = paste('Votes:', votes), y = MovieID, x = Year )) + 
  geom_tile(aes(fill = secuencia)) +                  
  scale_y_continuous(breaks=1:10) +             
  scale_x_continuous(breaks=1999:2005) +
  scale_fill_manual(values = mycolors) +              #secuencia de colores
  labs(fill = 'Votes') -> h                           #Legend name

library(plotly) ###### interactivos 
ggplotly(h, tooltip = c('text', 'MovieID', 'Year'))



###############################################################################################################
# 8. Distribución del score promedio por año de las 10 películas con mayor número de valoraciones.
# 
###############################################################################################################

head(movies_onfire)
ggplot(movies_onfire, aes(as.character(MovieID), Mean))+
  geom_violin(scale='area')

head(mpg)
