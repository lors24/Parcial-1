library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)

xp = 0.5
yp = abs(0.5-0.5) + rnorm(1,0,1)
sim.prueba <- data.frame(xp,yp)

k=3
error <-rdply(1000, function(i){                     
x <- 0:20/20
y <- abs(x-.5) + rnorm(length(x), 0 ,1)
sim.datos <- data.frame(id=0:20,x,y)
vecino.k.prueba <- kknn(y ~ x, 
                        train = sim.datos, 
                        test = sim.prueba,
                        k=k, kernel='rectangular')
error <- (fitted(vecino.k.prueba)-sim.prueba$y)^2
sesgo <- (fitted(vecino.k.prueba)/k)^2
var <- 1/k
data.frame(error = error, sesgo = sesgo, var = var)
})

error$sum <- error$sesgo+error$var+1

err.p <- mean(error$error)
err.p

data.frame(k =i, prueba = prueba)


errores.vmc <- ldply(c(1,3,6), function(i){
    
    vecino.k.prueba <- kknn(y ~ x, 
                            train = sim.datos, 
                            test = sim.prueba,
                            k=i, kernel='rectangular')
     prueba <- mean((fitted(vecino.k.prueba)-sim.prueba$y)^2)
     data.frame(k =i, prueba = prueba)
})

errores.vmc


preds.1 <- rdply(100, function(i){
    x <- 0:20/20
    y <- abs(x-.5) + rnorm(length(x), 0 ,1)
    sim.datos <- data.frame(id=0:20,x,y)
    sim.prueba <- data.frame(x = 0.5, y = rnorm(1,0,1))
    salida.1 <- ldply(c(1,3,6), function(k){  #probamos varios valores de p
        vecino.k.prueba <- kknn(y ~ x, 
                                train = sim.datos, 
                                test = sim.prueba,
                                k=k, kernel='rectangular')
        prueba <- fitted(vecino.k.prueba)
        data.frame(k=k, prueba = prueba)
    })
    salida.1
})

preds.res <- preds.1 %>% 
    group_by(k) %>%
    dplyr::summarise(esperado = mean(prueba), var = var(prueba), ecm = mean((prueba-0)^2)) %>%
    mutate(sesgo = (esperado-1)^2)

preds.res

preds.res.1 <- preds.res %>%
    gather(variable,valor, esperado:sesgo) %>%
    filter(variable!='esperado')


ggplot(preds.res.1, aes(x=k, y=valor, colour=variable, group=variable)) +
    geom_line() + geom_point()



