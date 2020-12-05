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
  summarise(Mean_Score = mean(Score), n = n()) %>%
  left_join(titles, by = "MovieID") %>%
  arrange(desc(Mean_Score))
  

release_year_score_avg <- scores %>%
  group_by(Release_Year) %>%
  summarise(mean_score = mean(Score), n = n())

scores2 <- scores %>% left_join(movie_score_avg, by = "MovieID")

ggplot(data = release_year_score_avg) +
  geom_point(mapping = aes(x = Release_Year, y = mean_score))


scores_day_week <- scores %>% mutate(Day_Week = weekdays(Date))
scores_day_week %<>% mutate(Is_Weekend = isWeekend(Date))

day_week_score_avg <- scores_day_week %>%
  group_by(Day_Week) %>%
  summarise(mean_score = mean(Score), n = n())

weekend_weekday_score_avg <- scores_day_week %>%
  group_by(Is_Weekend) %>%
  summarise(mean_score = mean(Score), n = n())

n_scores_weekend = weekend_weekday_score_avg  %>% filter(Is_Weekend == TRUE) %>% select(n)
n_scores = sum(weekend_weekday_score_avg$n)
n_scores_weekend_weekday_ratio = n_scores_weekend / n_scores #el 18% de las valoraciones son en fin de semana, que es menos que el 28% de días que son fin de semana


# 100 peliculas de cada fichero como mínimo

# describir el problema, evaluar los datos que tenemos 
# análisis descriptivo: 
  # agrupar media puntuación / nº pelis puntuadas por año lanzamiento, año, mes, semana, día, dia de la semana, fin de semana / entre semana, pelicula, usuario
# series temporales de como se acumulan las puntuaciones para una pelicula
  # usar gráficas interactivas
# distribución de cuantos usuarios evaluan cuantas pelis totales y diferentes
# sistemas de recomendación (lo veremos con Jotabe)
# clase Ricardo similaridad coseno (extra - opcional)



