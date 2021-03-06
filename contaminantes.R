mediacontaminante<-function(x,y,z){
  u<-read.csv(file="001.csv")
  
}
mediacontaminante  <- function(directory, pollutant, id = 1:332) {
  data = lapply(id, function(i) read.csv(paste(directory, "/", formatC(i, 
                                                                       width = 3, flag = "0"), ".csv", sep = ""))[[pollutant]])
  
  return(mean(unlist(data), na.rm = TRUE))
}
mediacontaminante("specdata", "nitrate", 1:23)


completos <- function(directory, id = 1:332) {
  nobs = numeric()
  for (i in id) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    nobs = c(nobs, sum(complete.cases(newRead)))
  }
  return(data.frame(id, nobs))
}
completos("specdata", 1:33)

corr <- function(directory, horizonte=0) {
  
  df = completos(directory)
  ids = df[df["nobs"] > horizonte, ]$id
  corrr = numeric()
  for (i in ids) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
cr <- corr("specdata", 150)
head(cr)