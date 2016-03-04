x <- runif(1,1,100)
x
if (x>1 & x<30) {
  y <- "Hola"
} else if (x>31 & x<50) {
  y <- "Que onda"
} else if (x>51 & x<60) {
  y <- "Que pedo wey"
} else if (x>61 & x<80) {
  y <- "Que hay"
} else if (x>81 & x<100) {
  y <- "Que pex"
}
y