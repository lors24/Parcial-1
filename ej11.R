install.packages("ISLR")
library(ISLR)

# Queremos predecir el salario de un beisbolitsta en funcion de varias estadisticas
# que describen su desempeño

head(Hitters)
dim(Hitters) #Tenemos informacion sobre 322 bateadores, con 20 variables

# Vemos si tenemos Missing Values

complete <- sapply(1:ncol(Hitters), function(i){
    s <- sum(complete.cases(Hitters[,i]))
})
casos.completos <- data.frame(names(Hitters), complete)
casos.completos

# Vemos que la unica variable en la que hay Missing Values es Salary,
# pero es justo la que queremos predecir

322-263
263/322

# Si quitamos los 59 NA nos quedariamos con el 80% de los datos originales

Hitters.1 <- Hitters[complete.cases(Hitters),]
row.names(Hitters.1) = NULL
Hitters.1$id = 1:nrow(Hitters.1)

# Apartamos una muestra de prueba de tamaño 100, el resto sera de entrenamiento

N <- 100
indices.entrena <- sample(Hitters.1$id, N)
hitters.entrena <-filter(Hitters.1, !(id %in% indices.entrena))
hitters.prueba <- filter(Hitters.1, id %in% indices.entrena)


