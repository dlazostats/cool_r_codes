# Basic smooth spline Tibi
#-------------------------
# Create a simple nonlinear example
set.seed(0)
n = 500
x = sort(runif(n,0,6*pi))
r = x*sin(x) 
y = r + rnorm(n)

plot(x,y,col="gray50")
lines(x,r,col="blue",lwd=2)

# Regression splines
library(splines)
knots = seq(2,16,length=6)
G = bs(x,knots=knots,degree=3)
regmod = lm(y~G)

plot(x,y)
lines(x,regmod$fitted,lwd=3,col="blue")
abline(v=knots,lty=3,lwd=3)

# Smoothing splines
dfs = c(7,25,250)
splinemod1 = smooth.spline(x,y,df=dfs[1])
splinemod2 = smooth.spline(x,y,df=dfs[2])
splinemod3 = smooth.spline(x,y,df=dfs[3])

par(mfrow=c(2,3))
plot(x,y,col="gray50",main=paste("df =",dfs[1]))
lines(splinemod1$x,splinemod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("df =",dfs[2]))
lines(splinemod2$x,splinemod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("df =",dfs[3]))
lines(splinemod3$x,splinemod3$y,col="blue",lwd=2)

# Kernel regression
bws = c(5,1,0.1)
kernmod1 = ksmooth(x,y,kernel="normal",bandwidth=bws[1])
kernmod2 = ksmooth(x,y,kernel="normal",bandwidth=bws[2])
kernmod3 = ksmooth(x,y,kernel="normal",bandwidth=bws[3])

#par(mfrow=c(1,3))
plot(x,y,col="gray50",main=paste("bandwidth =",bws[1]))
lines(kernmod1$x,kernmod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[2]))
lines(kernmod2$x,kernmod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[3]))
lines(kernmod3$x,kernmod3$y,col="blue",lwd=2)
