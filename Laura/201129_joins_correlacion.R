#29.11.20
#contenido#
library(tidyverse)
aux=read_tsv("Raw data/combined_data_1.txt", col_names = FALSE, n_max = 30000) #lectura de las primeras 10^4 filas
aux=aux%>% mutate(fila=row_number()) #añadir columna con número de fila
filas=grep(":",aux$X1) #buscar filas con ":", filas comienzo nueva pelicula
filas_ID= aux %>% filter( fila %in% filas ) 
IDs=unique(filas_ID$X1)
reps=diff(c(filas_ID$fila,max(aux$fila)+1))
length(reps)
dim(aux)
sum(reps)
scores = aux %>% mutate(ID1=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) )

#ahora borramos los datos de la última película por si se han cortado a medias
scores = scores %>% filter( scores$fila < filas_ID$fila[length(filas_ID$fila)-1] )

# Ahora arregloamos la variable X1, y separamos la fecha en año, mes y día
scores = scores %>% separate(X1,into = c("CustomerID","Score","Date"), sep = ",")
scores = scores %>% mutate(Date_copy = Date)  %>% separate(Date_copy, into = c("Year", "Month", "Day"), sep = "-")

#Renombramos y reordenamos las variables
scores <- rename(scores, MovieID = ID1, RowID = fila)
scores = scores %>% relocate(RowID, MovieID, CustomerID, Date, Year, Month, Day, Score)

#Quitamos los ":" de el campo MovieID
scores$MovieID <- scores$MovieID %>% str_replace(":", "")

# Cambiamos los tipos de variable necesarios

scores <- scores %>% mutate(across(c(RowID:CustomerID, Year:Score), as.integer))
summary(scores)

length(unique(scores$CustomerID)) #9619 usuarios distintos
table(scores$Score) # frecuencia puntuaciones
table(scores$MovieID) # frecuencia pelis
scores<-mutate(scores, fila=NULL) # eliminamos la columna fila
scores
#--------------------------------------------------------------------------------------#

#Base de datos relacionales
#library(readr)
#movie_titles <- read_csv("Raw data/movie_titles.csv", 
#                        col_names = FALSE)
#View(movie_titles)

#QUIZÁS SEA INTERESANTE INCLUIR LA DIFERENCIA ENTRE:
#Claves primarias y foráneas: 
  #primarias: no existen 2 clientes con el mismo identificador: CustomerID
    #MovieID, CustomerID, MovieTitle
  #foráneas: Year, Month, Day, Score, Date, Año publicación peli. #pueden asignarse a varios.

#comprobamos eso (en la lista movie_titles):
scores %>%
  count(MovieID, CustomerID, Date) %>%
  filter(n>1)
#En estos casos, la propia fila en si misma constituye la clave.

scores %>%
  count(MovieID, CustomerID) %>%
  filter(n>1)
#para esta combinación no hay dobles entradas #esto implica que las valoraciones por cliente son únicas
#en este caso, la clave primaria seria un concatenado de la MovieID y el CustomerID

#LOS MUTATING JOINS
library(readr)
movie_titles <- read_csv("Raw data/movie_titles.csv")
View(movie_titles)

#Para incorporar la columna a la tabla 'Scores' puede hacerse de dos maneras:
#1:
scores %>% 
  left_join(movie_titles,by = 'MovieID' ) -> scores

scores

#------------------------------------UDEMY EJEMPLO:
#INNER JOIN: Join interno, se encarga de buscar coincidencias exactas de las observaciones a través de las claves.
#Join de igualdades, las claves tienen que coincidir a la precisión.

x <- tribble(
   ~key, ~value_x,
      1, 'x1',
      2, 'x2',
      3, 'x3',
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
  4, 'y3',
)

x %>%
  inner_join(y, by = 'key') #las filas que NO coinciden, se excluyen directamente del resultado.

#OUTER JOIN: Esta modalidad supera el problema anterior y conserva las observaciones que aparecen, al menos, en una de las dos tablas
  #LEFT JOIN: Se queda con TODAS las observaciones que aparecen en el primer dataset. Nos interesa aplicar este porque queremos TODAS las peliculas de la tabla Score
  #RIGHT JOIN: Se queda con TODAS las observaciones que aparecen en el segundo dataset (movie_titles), independientemente de que aparezca o no en el primero.
  #FULL JOIN: Se queda con TODAS las observaciones que aparecen en el primero & segundo dataset

x %>%
  left_join(y, by = 'key')

x %>%
  right_join(y, by = 'key')

x %>%
  full_join(y, by = 'key')

#El problema de las claves duplicadas


x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  2, 'x3',
  1, 'x4',
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
)

    ##Claves duplicadas en una tabla
x %>% left_join(y, by = 'key')
table(x)
    ##Claves duplicadas en las dos tablas
#NO DEBERÍA PASAR, ES UN INDICADOR DE UN ERROR
x <- tribble(
  ~key, ~value_x,
  1, 'x1',
  2, 'x2',
  2, 'x3',
  3, 'x4',
)
y <- tribble(
  ~key, ~value_y,
  1, 'y1',
  2, 'y2',
  2, 'y3',
  3, 'y4',
)

left_join(x,y, by = 'key')
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

scores
scores %>% 
 select(Score, MovieTitle)

##PELICULAS CON 4 Y 5
scoremax = scores %>% 
  filter(Score %in% c(4,5))

test1 = scoremax %>% 
  select(MovieTitle, Score)

barplot(table(test1))
        
#COVARIACIÓN

#Categoría vs Contínua

library(ggplot2)
ggplot(data = scores, mapping = aes(x = MovieID))+
  geom_freqpoly(mapping = aes(color = Score), binwidth=1)


#como se reparten los 4 y los 5 de puntuación entre las peliculas
ggplot(test1) +
  geom_bar(mapping = aes(x = MovieID))

ggplot(test1) +
  geom_bar(mapping = aes(x = MovieID))

#covarianción a través de densidades
ggplot(data = scores, mapping = aes(x = MovieID, y = ..density..))+
  geom_freqpoly(mapping = aes(color = Score), binwidth=1)

#boxplots:
ggplot( data = scores, mapping = aes(x = Score , y = MovieID )) +
  geom_boxplot()

#Contínua vs Contínua
scores








