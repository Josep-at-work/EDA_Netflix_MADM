---
title: "Netflix movies"
date: "28/11/2020"
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
  


library(tidyverse)


## Carga de datos

aux=read_tsv("Raw data/combined_data_1.txt", col_names = FALSE, n_max = 30000) #lectura de las primeras 10^4 filas
aux=aux%>% mutate(fila=row_number()) #añadir columna con número de fila
filas=grep(":",aux$X1) #buscar filas con ":", filas comienzo nueva pelicula
filas_ID= aux %>% filter( fila %in% filas ) 
IDs=unique(filas_ID$X1)
reps=diff(c(filas_ID$fila,max(aux$fila)+1))
length(reps)
dim(aux)
sum(reps)
combined_data_1 = aux %>% mutate(ID1=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) )

#ahora borramos los datos de la última película por si se han cortado a medias
combined_data_1 = combined_data_1 %>% filter( combined_data_1$fila < filas_ID$fila[length(filas_ID$fila)-1] )

#combined_data_1 = combined_data_1 %>% filter(ID1==ID1[length(IDs)])

# El ultimo filtro hace que solo pille los datos para la peli con ID = 1
# filtro el último pues no sé si lo he leído entero y las entradas 1:

# Ahora arreglo la variable X1
combined_data_1= combined_data_1 %>% separate(X1,into=c("CustomerID","Score","Date"),sep=",")

#Renombramos y reordenamos las variables
combined_data_1 <- rename(combined_data_1, MovieID = ID1, RowID = fila)
combined_data_1 = combined_data_1 %>% relocate(RowID, MovieID, CustomerID, Date, Score)

#Quitamos los ":" de el campo MovieID
combined_data_1$MovieID <- combined_data_1$MovieID %>% str_replace(":", "")

# Cambiamos los tipos de variable necesarios
combined_data_1$MovieID = as.integer(combined_data_1$MovieID)
combined_data_1$CustomerID = as.integer(combined_data_1$CustomerID)
combined_data_1$Score = as.integer(combined_data_1$Score)




#rm(aux,filas,filas_ID,IDs,reps)

# Visualizo la tabla

# knitr::kable(combined_data_1) 

summary(combined_data_1)


df = aux %>% mutate(ID=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) )
df = df %>% separate(X1,into=c("ID_customer","Score","date"),sep=",") #separa por comas y da nombre a las columnas.
summary(df) # Las 10K filas menos 8(numero de IDs de las pelis)
