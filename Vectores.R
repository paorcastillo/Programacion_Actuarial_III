z<-5
i<-1
datos <-vector("numeric",0)
while(z>=3 && z<=10){
  
  moneda<-rbinom(1,1,0.5)
  length(datos)<-length(datos)
  if(moneda==1){
    z<-z+1
    datos[i]<-z
  }else{
    z<-z-1
    datos[i]<-z
  }
  i<-i+1
}
datos