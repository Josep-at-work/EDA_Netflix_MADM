ggplot(data = sample, aes(x = Release_Year)) + geom_bar() #gráfico de barras con la cantidad de películas por año

#ggplot(data = sample) + geom_area((aes(y = ..density..), stat = "bin")

library(ggplot2)
p <- ggplot(sample, aes(x=Release_Year))
## Basic area plot
p + geom_area(aes(y=MovieID)) #geom_area

## Basic density plots
p <- ggplot(sample, aes(x=Mean_Score)) + 
  geom_density()
p
# Add mean line
p+ geom_vline(aes(xintercept=mean(n)),
              color="blue", linetype="dashed", size=1)

## Histogram with density plot
ggplot(sample, aes(x=Release_Year)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

##Gráfico de barras (de 1 solo color)
ggplot(data = sample, aes(x = Release_Year)) + 
  geom_bar(color = 'chocolate4', fill = 'chocolate') + 
  xlab("Release Year") + 
  ylab("Number of Films") + 
  ggtitle("Bar Plot")+
  coord_flip() #cambiar orientación ejes

#I LIKE IT##Gráfico de barras (de varios colores)
ggplot(data = sample, aes(x = Release_Year, fill = as.factor(Release_Year))) + 
  geom_bar() + 
  xlab("Release Year") + 
  ylab("Number of Films") + 
  ggtitle("Bar Plot") +
  labs(fill = "Release Year") + 
  theme_test() #para cambiar el 'tema' de visualización

ggplot(sample) + 
  geom_histogram(bins = 7, aes(x = Mean_Score), fill = 'steelblue') + 
  xlab("Mean_Score") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de la variable Mean_Score") +
  theme_minimal()

sample$Mean_Score

##Gráfico de Dispersión (Scatterplot)
ggplot(data = sample, aes(x = Mean_Score, y = Release_Year)) + 
  geom_point(color = 'red', fill = 'red', size = 4, shape = 18, alpha = 0.5) +
  #geom_smooth(color = 'red') + #para poner línea de tendencia
  xlab('Mean_Score') + 
  ylab('Release_Year') +
  ggtitle('Relation between Mean_Score & Release_Year') + 
  theme_test()

#No hay correlación entre el año de estreno y la media. 
sample

##correlograms: Un coeficiente de correlación igual a -1 indica total anti-correlación entre 
#las variables, mientras que un coeficiente de correlación igual a 1 indica correlación total entre las variables. 
#Por supuesto, un valor igual a 0 indica que no existe correlación alguna entre las variables. 
library(ggcorrplot)
corr <- round(cor(sample[1:4]), 1) #excluir los títulos
corr

#como la matriz de correlación es simétrica, basta con graficar simplemente una parte de ella
ggcorrplot(corr, method = 'circle', type = 'lower', lab = TRUE) +
  ggtitle("Correlograma del conjunto sample") +
  theme_minimal() +
  theme(legend.position="none")
#observamos que las variables n (valoración abs) esta muy correlacionada con el MeanScore, lo cual es lógico.


## GRÁFICO 1: MAPA DE PELÍCULAS SELECCIONADAS.
library(treemapify)

(ptm<-ggplot(sample,aes(area=n,fill=Title,
                        subgroup = n, subgroup2 = round(Mean_Score,2),
                    label=Title))+
    ggtitle('Top 5 películas')+
    labs(caption = "Selección según el valor absoluto")+
    geom_treemap(color = '#A67B5B') +
    theme(legend.title = element_blank(),
          legend.position = "none")+
    geom_treemap_subgroup_text(place = "bottomright", 
                      grow = F,
                      alpha = 0.9,
                      size = 7) +
    geom_treemap_subgroup2_text(place = "bottomleft", 
                               grow = F,
                               alpha = 0.9,
                               size = 8) +
  geom_treemap_text(place = "centre",
                             grow = F,
                             alpha = 1,
                             colour = "#FAFAFA",
                            size = 10) +
    geom_treemap_subgroup_border(colour="black",size=1)+
    geom_treemap_subgroup2_border(colour="black",size=1)+
    theme(plot.background = element_rect(fill = "#FFFEF2")) )


