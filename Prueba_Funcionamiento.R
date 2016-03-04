#Prueba de funcionamiento
n <- 100000
mensajes <- vector("character",n)
for(i in 1:n) {
  y <- runif(1)
  if (y <= 0.30){
    mensajes[i] <- "Mensaje 30%"
  } else if (y <= .50){
    mensajes [i] <- "Mensaje 20% a"
  } else if (y <= .70){
    mensajes[i] <- "Mensaje 20% b"
  } else if (y <= .90){
    mensajes[i] <- "Mensaje 20% c"
  } else{
    mensajes[i] <- "Mensaje 10%"
  }
}
mensajes
plot(table(mensajes))