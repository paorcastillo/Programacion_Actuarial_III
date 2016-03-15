perro <- function(x){
  z <- 100
  w <- 1
  secuencia <- vector("numeric",0)
  for(i in 1:x){
    
    secuencia[i] <- z
    length(secuencia) <- length(secuencia)
    moneda <- runif(1)
    if(moneda <= 0.5){
      z <- z + 0.5
    } else {
      z <- z - 0.5
    }
    
  }
  secuencia
  plot(secuencia, type = "l")
}
