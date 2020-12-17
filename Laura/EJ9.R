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


















