## Inverse Transfrom Method
#--------------------------
## sampling from finite population
# toss a coin
sample(0:1,size=10,replace=T)

# multinomial distr
sample(1:3,size=100,replace=T, prob=c(0.2,0.3,0.5))

# continuous case
#===============
# example 1 :
# f(x)=3x^2
fo<-function(x) 3*x^2  #[0,1]
Fx<-function(x) x^3
F_inv<-function(u) u^(1/3)
m<-10000
u<-runif(m)
x<-F_inv(u)
hist(x,prob=T,main=expression(f(X)==3*x^2))
y<-seq(0,1,0.01)
lines(y,3*y^2)

# example 2 :
# f(x)=lambda*exp(-x*lambda)
Finv<-function(u,lambda) -log(u)/lambda
u<-runif(m)
x<-Finv(u,2)
hist(x,prob=T,main=expression(f(X)==2*e^(-2*x)))
y<-seq(0,5,0.01)
lines(y,2*exp(-2*y))

# Discrete case
#---------------
#example 1 : bernoulli 
n<-1000
p<-0.4
u<-runif(n)
x<-as.integer(u>0.6)
mean(x)
var(x)
mean(rbinom(n,size=1,prob=p))
