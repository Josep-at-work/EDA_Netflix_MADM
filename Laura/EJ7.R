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

table7 = head(table7[order(table7$votes, decreasing = TRUE),],10)

table7 ##AQUI MI RESULTADO

ggplot(table7, aes(x = Year, y = votes, fill = MovieID)) + geom_tile()

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

ggplot(df.heatmap, aes(x = Var1, y = Var2, fill = score)) + geom_tile() + scale_fill_gradient(low = "white", high = "steelblue")
