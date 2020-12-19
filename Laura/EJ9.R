#EJ9
##Primero me creo una tabla seleccionando los campos que quiero comparar
cols <- c(4,13)

newtab=scores[,cols]
newtab

##Aquí tenemos los tres Q separados:
Q1 = filter(newtab, Month == 'Jan' |Month == 'Feb' | Month == 'Mar' | Month == 'Apr')
Q2 = filter(newtab, Month == 'May' | Month == 'Jun'| Month == 'Jul' | Month == 'Aug')
Q3 = filter(newtab, Month == 'Sep' |Month == 'Oct' | Month == 'Nov' | Month == 'Dec')

## GRÁFICOS CON Etiquetas

## GRÁFICOS CON Etiquetas

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

#####################QUÉ PELÍCULAS HAN EVALUADO LOS 5 USUARIOS########################
#número de veces que ha votado cada usuario
num_votos_por_usuario = aggregate(scores$UserID, by = list(Usuario=scores$UserID), length)

#Los cinco usuarios que más películas han evaluado, qué peliculas son y qué notas les han dado:

#top_5_users= head(num_votos_por_usuario[order(num_votos_por_usuario$x, decreasing = TRUE),],5)
#top_5_users # 305344 , 387418, 2439493, 1664010, 2118461.


##idea: qué pelicula ha sido la menos evaluada

library(dplyr)
top_pelis = scores %>% 
              filter(UserID %in% c('305344', '387418', '2439493', '1664010', '2118461'))
#Títulos películas
unique(top_pelis$Title)
unique(scores$Title)
#Vemos que entre el total de películas puntuadas por estos 5 usuarios, se han puntuado todas las películas.

##Ahora, para ver las películas que ha evaluado cada usuario vamos a realizar lo siguiente:

##top_pelis_1
top_pelis_1 = scores %>% 
  filter(UserID %in% c('305344'))

##top_pelis_2
top_pelis_2 = scores %>% 
  filter(UserID %in% c('387418'))

##top_pelis_3
top_pelis_3 = scores %>% 
  filter(UserID %in% c('2439493'))

##top_pelis_4
top_pelis_4 = scores %>% 
  filter(UserID %in% c('1664010'))

##top_pelis_5
top_pelis_5 = scores %>% 
  filter(UserID %in% c('2118461'))


#Filas diferentes presentes en el top_pelis_1

#anti_join(top_pelis_1, top_pelis_2) #no me sirve porque tengo demasiados registros....

#El usuario que más películas ha puntuado es el 305344, entonces vamos a comparar el resto de usuarios con este:

#usuario top1 y top2: qué películas top 2 no ha evaluado que si lo haya realizado top 1
top_pelis_1$comp2 <- as.integer(top_pelis_1$MovieID %in% top_pelis_2$MovieID) #Usamos el operador %in% para ver si cada elemento de top_pelis_1 se encuentra en top_pelis_2. 

#Registros que SI estan en 1 pero no en 2
Dif_1_2= top_pelis_1 %>% 
           filter(comp2 %in% c('0')) #░obtener las que no aparecen
Dif_1_2 #Obtenemos las 3 películas que el usuario 2 no ha puntuado #PREGUNTA: un usuario puede puntuar una película dos veces?

#Entonces, el usuario que ocupa una 2 posición en la lista de usuarios que más películas han evaluado, ha puntuado tres títulos menos que el usuario 1:
Dif_1_2$Title

# A continuación, seguimos realizando lo mismo con el resto de usuarios (comparandolos siempre con el usuario que ha puntuado más peliculas):


top_pelis_1$comp3 <- as.integer(top_pelis_1$MovieID %in% top_pelis_3$MovieID)
top_pelis_1$comp4 <- as.integer(top_pelis_1$MovieID %in% top_pelis_4$MovieID)
top_pelis_1$comp5 <- as.integer(top_pelis_1$MovieID %in% top_pelis_5$MovieID)
View(top_pelis_1)

Dif_1_3= top_pelis_1 %>% 
  filter(comp3 %in% c('0')) #░obtener las que no aparecen
Dif_1_3 

Dif_1_4= top_pelis_1 %>% 
  filter(comp4 %in% c('0')) #░obtener las que no aparecen
Dif_1_4 

Dif_1_5= top_pelis_1 %>% 
  filter(comp5 %in% c('0')) #░obtener las que no aparecen
Dif_1_5 

#Otra manera de obtener cuantas veces puntua cada usuario

df <- scores %>% group_by(UserID) %>% count()
df <- scores %>% group_by(UserID) %>% summarise(NN = n())
df <- scores %>% group_by(UserID) %>% 
  summarise(NN = n(), percent = n()/nrow(.) ) #Añadir a la tabla el % que representa cada país en el Total
df <- scores %>% group_by(UserID) %>%
  summarise (NN = n()) %>%
  mutate(percent= NN / sum(NN))
top_5_users <- head(df[order(df$NN, decreasing = TRUE),],5)
knitr::kable(top_5_users)


#puntuacion media total estos clientes:
  #df <- scores %>%  group_by(UserID, Score) %>% 
  #summarise(Score_medio = mean(Score, na.rm = TRUE)) %>% ungroup() 