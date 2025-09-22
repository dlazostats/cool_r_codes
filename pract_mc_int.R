library(lattice)
library(dplyr)

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
#--------------------
#first example: int(0,10) exp(-2*abs(x-5))
#---------------------------------------
f<-function(x) exp(-2*abs(x-5))

# using Adaptive quadrature 
integrate(f,0,10)

# basic MC integration
n<-1000
xs<-runif(n,0,10)
Y<-(10)*f(xs)
histogram(Y)
mean(Y)
var(Y)  #quite large!!!!

# Importance sampling
# find the peak of a function
# lets study our funtion
res_opt<-optimize(f,interval=c(0,10),maximum=T)  
peak_x<-res_opt$maximum
peak_y<-res_opt$objective
x <- seq(0, 10, by = 0.1);y <- f(x)
plot(x, y, type = "l", main = "f(x) = exp(-2*abs(x-5))", 
     xlab = "x", ylab = "f(x)")
abline(v = 5, col = "red", lty = 2)  

# use this information to pick the g(x) function
n<-10000
x<-rnorm(n,5,1)
h<-function(x) 10*f(x)
w<-function(x) dunif(x,0,10)/dnorm(x,5,1)

Y=h(x)*w(x)
mean(Y)
var(Y) 

#second example: calculate the second moment E(x^2)
#--------------------------------------------------
p_2nd<-function(x) 0.5*(x^2)*exp(-abs(x))
x <- seq(-10, 10, by = 0.1);y <- p_2nd(x)
plot(x, y, type = "l", main = "f(x) = exp(-2*abs(x-5))", 
     xlab = "x", ylab = "f(x)")

# adaptative quadrature
integrate(p_2nd, -Inf, Inf)

# importance
# N(0,4)
n<-10000
x<-rnorm(n,0,4)
y<-function(x) p_2nd(x)/dnorm(x,sd=4)
mean(y(x))
var(y(x))

# N(0,1)
n<-10000
x<-rnorm(n)
y<-function(x) p_2nd(x)/dnorm(x)
mean(y(x))
var(y(x))

# Third example: from maria risso
#---------------------------------
g<-function(x) {exp(-x)/(1+(x^2))*(x > 0)*(x < 1)}
x<-seq(0,1,0.001)
y<-g(x)
plot(x,y,type="l")
lines(x,f1(x),col="red")

# r function
integrate(g,0,1)

# compare different importance functions
n<-10000
theta.hat <- se <- numeric(5)

# basic MC integration
x<-runif(n)
y<-g(x)
theta.hat[1]<-mean(y)
se[1]<-sd(y)

# importance sampling 
# exp
x<-rexp(n,1)
f1<-function(x) exp(-x)
yimp<-g(x)/f1(x)
theta.hat[2]<-mean(yimp)
se[2]<-sd(yimp)

#cauchy
x <- rcauchy(n)
i <- c(which(x > 1), which(x < 0))
x[i] <- 2
f2<-function(x) dcauchy(x)
yimp<-g(x)/f2(x)
theta.hat[3]<-mean(yimp)
se[3]<-sd(yimp)

# inverse transform method
u<-runif(n)
x<- -log(1-u*(1-exp(-1)))
yimp<-g(x)/(exp(-x)/(1-exp(-1)))
theta.hat[4]<-mean(yimp)
se[4]<-sd(yimp)

u<-runif(n)
x<- tan(pi*u/4)
yimp<-g(x)/(4/((1+x^2)*pi))
theta.hat[5]<-mean(yimp)
se[5]<-sd(yimp)

rbind(theta.hat, se/sqrt(n)) %>% as.data.frame() %>% 
  rename("MC basico"="V1","Exp"="V2","Cauchy"="V3","InvTran1"="V4","InvTran2"="V5") ->res_comp
rownames(res_comp)<-c("Integral","SE")
res_comp

plot(x,y,type="l",ylim=c(0,2))
lines(x,f1(x),col="red")
lines(x,dunif(x),col="blue")
lines(x,dcauchy(x),col="green")
lines(x,exp(-x)/(1-exp(-1)),col="gold")

