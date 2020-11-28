#28.11.20

###Ricardo
library(tidyverse)

#probe=read_csv("Raw data/probe.txt")
aux=read_tsv("Raw data/combined_data_1.txt",col_names=FALSE,n_max =20000)# leo 10^4 lo leo con tabulador para que lea una varable
aux=aux%>% mutate(fila=row_number())
filas=grep(":",aux$X1)
filas_ID= aux %>% filter( fila %in% filas )
IDs=unique(filas_ID$X1)
reps=diff(c(filas_ID$fila,max(aux$fila)+1))
length(reps)
dim(aux)
sum(reps)
combined_data_1=aux %>% mutate(ID1=rep(filas_ID$X1,times=reps)) %>% filter(!(fila %in% filas) ) %>% filter(ID1==ID1[length(IDs)])
# filtro el último pues no sé si lo he leído entero y las entradas 1:

# Ahora arreglo la variable X1

combined_data_1= combined_data_1 %>% separate(X1,into=c("ID_customer","Score","date"),sep=",")
rm(aux,filas,filas_ID,IDs,reps)

# Visualizo la tabla

# knitr::kable(combined_data_1) 

summary(combined_data_1)

###TRANSFORMACIÓN DE DATOS
#PAQUETE dplyr
library(dplyr)
#Dataset:
View(combined_data_1) #Un tibble es un data frame tuneado para trabajar mejor con el resto de herramientas de Tidyverse
## * int -> números enteros
## * dbl -> números reales (double)
## * chre -> vector de carácteres o strings
## * dttm -> date + time
## * lgl -> logical, contiene valores booleanos (T o F)
## * fctr -> factor, variables categóricas
## * date -> fecha (día, mes y año)

## * filter() -> filtrar observaciones a partir de valores concretos
## * arrange() -> reordenar las filas
## * select() -> seleccionar la variable por sus nombres
## * mutate() -> crea nuevas variables con funciones a partir de las existentes
## * summarise() -> colapsar varios valores para dar un resumen de los mismos

## * group_by() -> opera la función a la que acompaña grupo a grupo

##1 - data frame
##2 - operaciones que queremos hacer a las variables del data frame
##3 - resultado en un nuevo data frame

### FILTER (da como resultado un nuevo data frame construido desde 0, no se carga el original)

Score1 = filter(combined_data_1, Score ==1)#quiero obtener todos los clientes que puntuaron la película 1 con un score igual a 1
Score2 = filter(combined_data_1, Score ==2)#"" y lo introduzco en una variable
Score3 = filter(combined_data_1, Score ==3)#""
Score4 = filter(combined_data_1, Score ==4)#""
(Score5 = filter(combined_data_1, Score ==5))#"" Si lo ejecuto, me sale tanto en consola como en el Environment.
# >, >=, <, <=, ==, !=

#Si miramos la pantalla de 'Environment' podemos ver que, 
#la puntuación por excelencia para la película 1
#es de '4', seguida por'5', '3','2' y '1'.

###EL ÁLGEBRA DE BOOL EN EL FILTRADO

filter(combined_data_1, Score == 5 | Score == 4)#Usuarios que han puntuado la película con un Score igual a `5` o `4`

Score_4_5 <- filter(combined_data_1, Score %in% c(4,5)) #otra manera de obtener lo de arriba

#Ley de Morgan
#!(x&y) == (!x)|(!y)
#!(x|y) == (!x)&(!y)

#Clientes cuyo Score fue inferior a 4
filter(combined_data_1, !(Score >=4))

###Comprobar si alguna columna recoge algún NA
is.na(combined_data_1)
is.na(combined_data_1$ID_customer)

#Las 10 primeras y últimas filas
head(combined_data_1, 10)
tail(combined_data_1, 10)

### ARRANGE (de Mayor a Menor)
sorted_date = arrange(combined_data_1,date)
head(sorted_date) #el dato más antiguo es de Enero/2004
tail(sorted_date) #el dato más novedoso es de Diciembre/2005
arrange(combined_data_1, desc(ID_customer)) #no me lo ordena bien
arrange(combined_data_1, Score, date) #primero la ordena según el Score y luego por su date

arrange(combined_data_1, desc(date)) #ordenar de más novedoso a menos novedoso

### SELECT

arrange(combined_data_1, Score)
sorted_date[1,] #Devolver la primera fila, de todas las columnas, ordenado por la fecha
sorted_date[,1] #Devolver la primera columna, ordenado por la fecha

select(sorted_date[1:100,], ID_customer, Score, ID1, date) #así no selecciono la columna 'fila' pero solo es para las 100 primeras filas

select(combined_data_1,ID_customer:date) #Seleccionar las columnas desde el 'ID_customer' hasta 'date'

select(combined_data_1, -(fila))#todas las columnas menos la columna 'fila'

select(combined_data_1, starts_with('ID')) #me quedo con las columnas que empiezan por 'ID'

select(combined_data_1, ends_with("te")) #me quedo con las columnas que terminan por 'te'.

select(combined_data_1, contains('s')) #me quedo con las columnas que contienen 's'.

select(combined_data_1, matches('(.)\\1')) #expresiones regulares: me busca carácteres repetidos en las columnas.

select(combined_data_1, num_range('ID', 0:9)) #Columnas que empiecen por ID (en su cabecera) y contengan valores entre el 0 y el 9

#RENOMBRAR Y ORDENAR LAS COLUMNAS

renamed = rename(combined_data_1, Film_1=ID1, Date=date, Row=fila, ID_Customer=ID_customer) #nombre nuevo seguido del antiguo

select(renamed,Film_1, Score, Date, everything()) #especifico el orden inicial y luego, el `everything` me coloca el resto de columnas sin con su orden natural.


### MUTATE: CALCULAR NUEVAS VARIABLES A PARTIR DE LAS QUE YA TENEMOS

renamed_new <- select(renamed,
                      Date,
                      Score,
                      ID_Customer,
                      Film_1,
                      Row)
mutate

attach(renamed_new)
Score = as.numeric(Score)

class(Score)

Score
renamed_new = mutate(renamed_new,
          Score = as.integer(Score),
          Mean_Score = mean(Score),
          Mean_Score_Distance = Score - mean(Score), #cómo de lejos esta la puntuación del usuario de la puntuación media de la peli
          Percent_Of_Total = ((Score / sum(Score)))*100)

View(renamed_new)

renamed_new
renamed_new = renamed_new%>% separate(Date,into=c("Year","Month","Day"),sep="-")
renamed_new

renamed_new$Month[Score == max(Score)]
renamed_new$Year[Score == max(Score)]
renamed_new$Day[Score == max(Score)]

#En el mutate se suelen aplicar:
# * Operaciones aritméticas: +, -, *, /, ^ (hours + 60* minutes)
# * Agregados de funciones: x/sum(x) : proporcion sobre el total
#                           x - mean(x): distancia respecto de media
#                           (x-mean(x))/sd(x): tipificación
#                           (x-min(x))/max(x): estandarizar entre [0,1]
# * Aritmética modular: %/%-> cociente de la división entera, %% -> resto de la división entera
#                           x == y * (x%/%y) + (x%%y)
# * Logaritmos: log() -> logaritmo en base e, log2(), log10()
# * Offsets: lead(), lag()

