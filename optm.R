## Optimization
#----------------
## Example 1
f <- function(x) -exp(-( (x-2)^2 ))
plot(seq(-1,1,0.01),f(seq(-1,1,0.01)),type="l")

## without derivative
optim(1,f)$par

df<-function(x) -2*(x-2)*f(x)
optim(1,f,df,method="CG")$par

# Two dimensional examples
f <- function(x1,y1) (1-x1)^2 + 100*(y1 - x1^2)^2
x <- seq(-2,2,by=.15)
y <- seq(-1,3,by=.15)
z <- outer(x,y,f)
persp(x,y,z,phi=45,theta=-45,col="yellow",shade=.00000001,ticktype="detailed")

f <- function(x) (1-x[1])^2 + 100*(x[2]-x[1]^2)^2
optim( c(0,0), f )$par

# Newton method
# f and f’
f <- function(x) .5-exp(-(x^2))
df <- function(x) 2*x*exp(-(x^2))
v <- seq(0,2,length=1000) # make the initial plot
plot(v,f(v),type="l",col=8)
abline(h=0)
x0 <- 1.5

# paste this repeatedly to see Newton’s method work
for(i in 1:10){
  segments(x0,0,x0,f(x0),col=2,lty=3)
  slope <- df(x0)
  g <- function(x) f(x0) + slope*(x - x0)
  v = seq(x0 - f(x0)/df(x0),x0,length=1000)
  lines(v,g(v),lty=3,col=4)
  x0 <- x0 - f(x0)/df(x0)
}

