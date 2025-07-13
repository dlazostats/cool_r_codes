## Jackknife
#------------
library(bootstrap)

# fun jack by hand
fun_jack<-function(x){
  rs<-numeric()
  n<-length(x)
  for(k in 1:length(x)){
    vd<-x[-k]
    s<-mean(vd)
    rs[k]<-s
  }
  return(sqrt(((n - 1)/n)*sum((rs - mean(rs))^2)))
}

# example1
x <- rnorm(20)               
theta <- function(x){mean(x)}
results <- jackknife(x,theta)    
mean(x)
sd(x)
sd(x)/sqrt(length(x))
fun_jack(x)
sd(bootstrap(x,1000,theta)$thetastar)

#example2
x<-mtcars$mpg
jackknife(x,theta)
fun_jack(x)
sd(bootstrap(x,1000,theta)$thetastar)
sd(x)/sqrt(length(x))
