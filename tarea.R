tarea <- function(variables=1000,escenarios=1000,fun="rnorm"){
  if(fun=="rgamma"){
    y <- sapply(lapply(rep(variables,escenarios),fun,0.5),mean)
    hist(y)
  }else{
    x <- sapply(lapply(rep(variables,escenarios),fun),mean)
    hist(x) 
  }
}