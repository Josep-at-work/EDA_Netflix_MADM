---
title: "Netflix movies"
date: "28/11/2020"
---
  


#ggplots

#scores vs release date

ggplot(data = scores) +
  geom_point(mapping = aes(x = Release_Year, y = Score ))

ggplot(data = scores) +
  geom_boxplot(mapping = aes(x = Release_Year, y = Score, group = Release_Year))


movie_score_avg <- scores %>%
  group_by(MovieID) %>%
  summarise(mean_score = mean(Score), n = n())

release_year_score_avg <- scores %>%
  group_by(Release_Year) %>%
  summarise(mean_score = mean(Score), n = n())

scores2 <- scores %>% left_join(movie_score_avg, by = "MovieID")

ggplot(data = release_year_score_avg) +
  geom_point(mapping = aes(x = Release_Year, y = mean_score))



# 100 peliculas de cada fichero como mínimo

# describir el problema, evaluar los datos que tenemos 
# análisis descriptivo: 
  # agrupar media puntuación / nº pelis puntuadas por año lanzamiento, año, mes, semana, día, dia de la semana, fin de semana / entre semana, pelicula, usuario
# series temporales de como se acumulan las puntuaciones para una pelicula
  # usar gráficas interactivas
# distribución de cuantos usuarios evaluan cuantas pelis totales y diferentes
# sistemas de recomendación (lo veremos con Jotabe)
# clase Ricardo similaridad coseno (extra - opcional)



