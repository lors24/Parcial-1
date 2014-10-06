#100 muestras de entrenamiento de tamaño 30


##
# conjunto de prueba 
xp <- runif(50)
yp <- abs(xp-0.5) + rnorm(length(xp), mean = 0, sd = 1)
datos.prueba <- data.frame(xp, yp)

salida.sim.lista <- lapply(1:100, function(i){
    x <- runif(30)
    y <- abs(x-0.5) + rnorm(length(x), mean = 0, sd = 1)
    datos <- data.frame(x,y)
    f.hat <- kknn(y ~ x, 
                  train = datos, 
                  test = datos,
                  k = 3, kernel = 'rectangular')
    error.entrena <- mean((fitted(f.hat) - datos$y)^2)
  
    # datos de prueba
    
    
    f.hat2 <- kknn(yp ~ xp,
                  train = datos,
                  test = datos.prueba,
                  k = 3, kernel = 'rectangular')
    error.prueba <- mean((fitted(f.hat2)-datos.prueba$yp)^2)     
    
    data.frame(muestra = i,error.entrena, error.prueba)
})

salida.sim <- rbind_all(salida.sim.lista)
sal.m <- melt(salida.sim, id.vars='muestra')

ggplot(sal.m,
       aes(x = muestra, y = value, col = variable, group = variable)) + geom_line() +
    geom_point()








