mejor <- function(estado,resultado){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  x <- levels(factor(data[,7]))
  v <- c("infarto", "falla", "neumonia")
  
  if (estado %in% x == F){
    stop("estado inválido")
    break
  }
  if (resultado == "infarto") r <- 11
  else if (resultado == "falla") r <- 17
  else if (resultado == "neumonia") r <- 23
  else if (resultado %in% v == F){
    stop("resultado inválido")
    break
  }
  mydata <- data[data$State == estado,]
  mnd <- mydata[,c(2,r)]
  if (sum(mnd[,2]=="Not Available") < 1) {
    out <- mnd[order(as.numeric(mnd[,2])),]
    out2 <- out[which(out[,2] == out[1,2]),]
    fo <- out2[order(out2[,1]),]
    fo[1,1]
    
  }
  else {
    final <- mnd[- grep("Not", mnd[,2]),]
    out <- final[order(as.numeric(final[,2])),]
    out2 <- out[which(out[,2] == out[1,2]),]
    fo <- out2[order(out2[,1]),]
    fo[1,1]
  }
}

mejor("TX", "infarto")
mejor("TX", "falla")
mejor("MD", "infarto")
mejor("MD", "neumonia")
mejor("BB", "infarto")
mejor("NY", "infartu")

rankhospital <- function(estado, resultado, num = "mejor"){
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  x <- levels(factor(data[,7]))
  v <- c("infarto", "falla", "neumonia")
  
  if (estado %in% x == F){
    stop("estado inválido")
    break
  }
  if (resultado == "infarto") r <- 11
  else if (resultado == "falla") r <- 17
  else if (resultado == "neumonia") r <- 23
  else if (resultado %in% v == F){
    stop("resultado inválido")
    break
  }
  mydata <- data[data$State == estado,]
  mnd <- mydata[,c(2,r)]
  if (sum(mnd[,2]=="Not Available") < 1) {
    
    out <- mnd[order(as.numeric(final[,2])),]
    if (num == "mejor") num <- 1
    else if (num == "peor") num <- nrow(out)
    else if (num > nrow(out)) {
      stop(return(NA))
    }
    i <- 0
    while (out[i+1,2] != out[num,2]){
      i <- i + 1
    }
    dif <- num - i
    out2 <- out[which(out[,2] == out[num,2]),]
    fo <- out2[order(out2[,1]),]
    fo[dif,1] 
  }
  
  else  {
    final <- mnd[- grep("Not", mnd[,2]),]
    out <- final[order(as.numeric(final[,2])),]
    if (num == "mejor") num <- 1
    else if (num == "peor") num <- nrow(out)
    else if (num > nrow(out)) {
      stop(return(NA))
    }
    i <- 0
    while (out[i+1,2] != out[num,2]){
      i <- i + 1
    }
    dif <- num - i
    out2 <- out[which(out[,2] == out[num,2]),]
    fo <- out2[order(out2[,1]),]
    fo[dif,1]
  }
}
rankhospital("TX", "falla", 4)
rankhospital("MD", "infarto", "peor")
rankhospital("MN", "infarto", 5000)

rankingcompleto <- function(resultado, num = "mejor") {
  data <- read.csv("outcome-of-care-measures.csv")
  v <- c("infarto", "falla", "neumonia")
  if (resultado == "infarto") col <- 11
  else if (resultado == "falla") col <- 17
  else if (resultado == "neumonia") col <- 23
  else if (resultado %in% v == F){
    stop("resultado inválido")
  }
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  
  output <- vector() # Vector a llenar (por filas) mediante el ciclo
  estados <- levels(data[, 7])
  for(i in 1:length(estados)) {
    databystate <- data[grep(estados[i], data[,7]), ]
    ordered <- databystate[order(databystate[, col], databystate[, 2], na.last = NA), ]
    
    if(num == "mejor") hospital <- ordered[1, 2]
    else if(num == "peor") hospital <- ordered[nrow(ordered), 2]
    else hospital <- ordered[num, 2]
    
    output <- append(output, c(hospital, estados[i]))
  }
  output <- as.data.frame(matrix(output, length(estados), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- estados
  output
}

head(rankingcompleto("infarto", 20), 10)
tail(rankingcompleto("neumonia", "peor"), 3)
tail(rankingcompleto("falla"), 10)




yt<-function(x){
  x>31

  }
  