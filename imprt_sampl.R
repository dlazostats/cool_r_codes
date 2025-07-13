## Importance Sampling
#---------------------
## Naive MC Integration
## example 1
### I = [0,10] exp(-2*abs(x-5))
f=function(x) exp(-2*abs(x-5))
plot(density(f(seq(0,10,0.01))))

integrate(f,0,10)
nmc_int<-function(f,m,a,b){
 x<-runif(m,a,b) 
 g<-(b-a)*f(x)
 return(list(int=mean(g),var=var(g)))
}
m<-10000
nmc_int(f,m,0,10)

## Importance sampling
## using X~N(5,1), h(x)
## x~unif(0,10), p(x) = 1/10  g(x)= 1/sqrt(2*pi) exp(0.5*(x-5)^2)
## h(x)[p(x)/g(x)]g(x)
w<-function(x)  dunif(x, 0, 10)/dnorm(x, mean=5, sd=1)
f<-function(x) exp(-2*abs(x-5))
x=rnorm(m,5,1)
y=10*w(x)*f(x) # 10 por p(x)
c(mean(y),var(y))
