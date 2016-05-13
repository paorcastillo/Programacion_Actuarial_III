#Teorema del limite central

a <- runif(1000,50,100)
hist(a)
hist(rnorm(1000,50,100))
hist(rexp(10000,1))
hist(rgamma(1000000,5,0.5))

n <- 100
sumas <- vector("numeric",n)
for(i in 1:n){
  sumas[i] <- sum(rexp(n),1)
}
hist(sumas)

a <- 100
b <- 10
prom <- sapply(lapply(rep(a,b), runif), mean)
hist(prom)