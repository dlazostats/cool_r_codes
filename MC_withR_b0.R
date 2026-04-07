# basic Montecarlo with R
#-------------------------
Nsim=10^4 #number of random numbers
x=runif(Nsim)
x1=x[-Nsim] #vectors to plot
x2=x[-1] #adjacent pairs
par(mfrow=c(1,3))
hist(x)
plot(x1,x2)
acf(x)
