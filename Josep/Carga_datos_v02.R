rm(list=ls())
cat("\014")

setwd("~/Developer/GitHub/proyecto-netflix-movies-madm/Data")

library(tidyverse)

filas_ID_combined_all = read.csv("filas_ID_combined_all.txt")
glimpse(filas_ID_combined_all)
table(filas_ID_combined_all$data)
#el set.seed y sample se tinene que ejecutar en la misma chunk porque solo se usa la seed una vez
###################################################
set.seed(080910)
# runif(4)
muestra_grupo = sample(1:12000, 250, replace=F)
head(muestra_grupo)
##################################################todo esto junto

pelis <- filas_ID_combined_all[as.vector(muestra_grupo),]
attach(pelis)

data1 = read_tsv("combined_data_1.txt",col_names = FALSE)
data2 = read_tsv("combined_data_2.txt",col_names = FALSE)
data3 = read_tsv("combined_data_3.txt",col_names = FALSE)
data4 = read_tsv("combined_data_4.txt",col_names = FALSE)

scores = tibble()
for(i in 1:nrow(pelis)){
  if (data[i]==1){
    scores = rbind(scores,data1[fila[i]:fila_final[i],])
  }
  if (data[i]==2){
    scores = rbind(scores,data2[fila[i]:fila_final[i],])
  }
  if (data[i]==3){
    scores = rbind(scores,data3[fila[i]:fila_final[i],])
  }
  else {
    scores = rbind(scores,data4[fila[i]:fila_final[i],])
  }
}  
scores
write_csv(scores,"nuestras_pelis.csv")
