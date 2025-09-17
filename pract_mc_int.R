library(lattice)

#example 1
h<-function(x) x^3
integrate(h,0,1)
n<-1000
x<-runif(n,0,1)
mean(h(x))

# example 2 acumulado
f<-function(x) (1/sqrt(2*pi))*exp(-(x^2)/2)
n<-10000
x<-rnorm(n,0,1)
xi=2
mean(x<=xi)
pnorm(2)

# example 3
x<-8
y<-6
n=m=10

fun_alph<-function() rbeta(1,y+1,m-y+1)-rbeta(1,x+1,n-x+1)
sim_alph<-replicate(1000,fun_alph())
histogram(sim_alph)
quantile(sim_alph,probs=c(0.025,0.975))

#importance sampling
