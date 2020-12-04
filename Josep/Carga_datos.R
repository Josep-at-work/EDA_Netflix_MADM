---
  title: "Treball"
author: "Josep"
date: "20/11/2020"
output: html_document
---

# The first line of each file contains the movie id followed by a
# colon. Each subsequent line in the file corresponds to a rating from a customer
# and its date in the following format:
# CustomerID,Rating,Date
#
#   -MovieIDs range from 1 to 17770 sequentially.
#   -CustomerIDs range from 1 to 2649429, with gaps. There are 480189 users.
#   -Ratings are on a five star (integral) scale from 1 to 5.
#   -Dates have the format YYYY-MM-DD.
#


library(tidyverse)
library(magrittr)
rm(scores, title)

## Carga de datos


#probe=read_csv("Raw data/probe.txt")
aux=read_tsv("Raw data/combined_data_1.txt",col_names=FALSE,n_max =10000)# leo 10^4 lo leo con tabulador para que lea una varable
aux=aux%>% mutate(fila=row_number())
filas=grep(":",aux$X1)
filas_ID= aux %>% filter( fila %in% filas )
IDs=unique(filas_ID$X1)
reps=diff(c(filas_ID$fila,max(aux$fila)+1))
length(reps)
dim(aux)
sum(reps)

scores = aux %>% mutate(ID1=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) )

#ahora borramos los datos de la última película por si se han cortado a medias
scores = scores %>% filter( scores$fila < filas_ID$fila[length(filas_ID$fila)-1] )

# Ahora arreglamos la variable X1, y separamos la fecha en año, mes y día
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
scores <- select(scores, -RowID) # eliminamos la columna fila

length(unique(scores$CustomerID)) #4834 usuarios distintos
table(scores$Score) # frecuencia puntuaciones
table(scores$MovieID) # frecuencia title

rm(titles,tt)
titles = read_csv('Raw data/movie_titles.csv', col_names=F)
tt<-tibble(titles)
head(tt)
tt <- rename(tt, MovieID = X1, Release_Year = X2, Title = X3)
summary(tt)

#Left Join
scores %<>% left_join(tt, by = 'MovieID') 
head(scores)

