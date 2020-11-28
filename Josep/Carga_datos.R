---
  title: "Netflix"
author: "Ricardo"
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
# rm(list)

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
combined_data_1=aux %>% mutate(ID1=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) ) %>% filter(ID1==ID1[length(IDs)])
# Que hace el ! en el filter de las filas.
# El ultimo filtro hace que solo pille los datos para la peli con ID = 1
# filtro el último pues no sé si lo he leído entero y las entradas 1:


# Ahora arreglo la variable X1

combined_data_1= combined_data_1 %>% separate(X1,into=c("ID_customer","Score","date"),sep=",")
#rm(aux,filas,filas_ID,IDs,reps)

# Visualizo la tabla

# knitr::kable(combined_data_1) 

summary(combined_data_1)


df = aux %>% mutate(ID=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) )
df = df %>% separate(X1,into=c("ID_customer","Score","date"),sep=",") #separa por comas y da nombre a las columnas.
summary(df) # Las 10K filas menos 8(numero de IDs de las pelis)
