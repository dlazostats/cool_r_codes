library(dplyr)

setwd("C:/Users/dlazo/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/Diego/cool_r_codes")

load("nonlin.Rdata")
# Plot the first training set
x = xtrain[,1]
y = ytrain[,1]
plot(x,y)
lines(x0,r0,lwd=2)

# Look at linear regression the first training set
linmodel = lm(y~x)
plot(x,y)
abline(a=linmodel$coef[1],b=linmodel$coef[2],col="red",lwd=2)

# Look at knn regression on the first training set
library(FNN)
ks = c(3,15,30)
knnmodel1 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[1])
knnmodel2 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[2])
knnmodel3 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[3])

par(mfrow=c(1,3))
plot(x,y,main=paste("k =",ks[1]))
lines(x0,knnmodel1$pred,col="red",lwd=2)
plot(x,y,main=paste("k =",ks[2]))
lines(x0,knnmodel2$pred,col="green4",lwd=2)
plot(x,y,main=paste("k =",ks[3]))
lines(x0,knnmodel3$pred,col="blue",lwd=2)

# Examine training and test errors
errtrain.lin = sum((y - linmodel$fitted)^2)
errtest.lin = sum((ytest[,1] - predict(linmodel,newx=xtest[,1]))^2)

nk = 60
ks = 1:nk
errtrain.knn = errtest.knn = numeric(nk)
for (i in 1:nk) {
  knnmodel = knn.reg(matrix(x,ncol=1),matrix(x,ncol=1),y=y,k=ks[i])
  errtrain.knn[i] = sum((y-knnmodel$pred)^2)
  knnmodel = knn.reg(matrix(x,ncol=1),matrix(xtest[,1],ncol=1),y=y,k=ks[i])
  errtest.knn[i] = sum((ytest[,1]-knnmodel$pred)^2)
}

ylim = range(c(errtrain.knn,errtest.knn))

par(mfrow=c(1,1))
plot(ks,errtrain.knn,type="l",ylim=ylim,col="red",
     main="Training and test errors",xlab="k",ylab="Error")
abline(h=errtrain.lin,lty=2,col="red")
lines(ks,errtest.knn,col="blue")
abline(h=errtest.lin,lty=2,col="blue")
legend("topleft",col=c("red","red","blue","blue"),lty=c(1,2,1,2),
       legend=c("KNN training error","LS training error",
                "KNN test error","LS test error"))

# Average these results over the 50 simulations
p = 50
errtrain.lin.all = numeric(p)
errtest.lin.all = numeric(p)
errtrain.knn.all = matrix(0,nk,p)
errtest.knn.all = matrix(0,nk,p)
for (j in 1:p) {
  cat(paste(j,",",sep=""))
  x = xtrain[,j]
  y = ytrain[,j]
  
  linmodel = lm(y~x)
  errtrain.lin.all[j] = sum((y - linmodel$fitted)^2)
  errtest.lin.all[j] = sum((ytest[,j] - predict(linmodel,newx=xtest[,j]))^2)
  
  for (i in 1:nk) {
    knnmodel = knn.reg(matrix(x,ncol=1),matrix(x,ncol=1),y,k=ks[i]) 
    errtrain.knn.all[i,j] = sum((ytrain[,j]-knnmodel$pred)^2)
    knnmodel = knn.reg(matrix(x,ncol=1),matrix(xtest[,j],ncol=1),y,k=ks[i])
    errtest.knn.all[i,j] = sum((ytest[,j]-knnmodel$pred)^2)
  }
}

errtrain.lin.ave = mean(errtrain.lin.all)
errtest.lin.ave = mean(errtest.lin.all)
errtrain.knn.ave = rowMeans(errtrain.knn.all)
errtest.knn.ave = rowMeans(errtest.knn.all)
ylim = range(c(errtrain.knn.ave,errtest.knn.ave))

par(mfrow=c(1,1))
plot(ks,errtrain.knn.ave,type="l",ylim=ylim,col="red",
     main="Averaged training and test errors",xlab="k",ylab="Error")
abline(h=errtrain.lin.ave,lty=2,col="red")
lines(ks,errtest.knn.ave,col="blue")
abline(h=errtest.lin.ave,lty=2,col="blue")
legend("topleft",col=c("red","red","blue","blue"),lty=c(1,2,1,2),
       legend=c("KNN ave training error","LS ave training error",
                "KNN ave test error","LS ave test error"))

# the true about linear regression 
# Figure 1
x1 = runif(100)
x2 = rnorm(100,0.5,0.1)
x3 = runif(100,2,3)
y1 = sqrt(x1) + rnorm(100,0,0.05)
y2 = sqrt(x2) + rnorm(100,0,0.05)
y3 = sqrt(x3) + rnorm(100,0,0.05)
plot(x1,y1,xlim=c(0,3),ylim=c(0,3))
rug(x1,side=1)
rug(y1,side=2)
points(x2,y2,pch=24,col="blue")
rug(x2,side=1,col="blue")
rug(y2,side=2,col="blue")
points(x3,y3,pch=22,col="red")
rug(x3,side=1,col="red")
rug(y3,side=2,col="red")
lm1 = lm(y1 ~ x1)
lm2 = lm(y2 ~ x2)
lm3 = lm(y3 ~ x3)
# abline takes intercept and slope as its first two arguments...
abline(lm1$coefficients)
abline(lm2$coefficients,col="blue")
abline(lm3$coefficients,col="red")
x.all=c(x1,x2,x3)
y.all=c(y1,y2,y3)
lm.all = lm(y.all~x.all)
abline(lm.all$coefficients,lty=2)
curve(sqrt(x),col="grey",add=TRUE)


# Figure 2: Make the 3D plot to show omitted variable bias
library(lattice)
library(MASS)  # for multivariate normal generator

# Make correlated normal variables X and Z
x.z = mvrnorm(100,c(0,0),matrix(c(1,0.1,0.1,1),nrow=2))
# Y = X+Z + small noise
y = x.z[,1] + x.z[,2] + rnorm(100,0,0.1)
# 3D scatterplot
cloud(y~x.z[,1]*x.z[,2],xlab="X",ylab="Z",zlab="Y")

# Figure 3
# Change the correlation between X and Z to -0.1 instead of +0.1
new.x.z = mvrnorm(100,c(0,0),matrix(c(1,-0.1,-0.1,1),nrow=2))
new.y = new.x.z[,1] + new.x.z[,2] + rnorm(100,0,0.1)
cloud(new.y~new.x.z[,1]*new.x.z[,2],xlab="X",ylab="Z",zlab="Y")

# Figure 4
# Now omit Z and plot
plot(x.z[,1],y,xlab="x",xlim=range(c(x.z[,1],new.x.z[,1])),ylim=range(c(y,new.y)))
# Make sure the range encompasses both data sets!
rug(x.z[,1],side=1)
axis(y,side=2)
points(new.x.z[,1],new.y,col="blue")
rug(new.x.z[,1],side=1,col="blue")
rug(new.y,side=2,col="blue")
# ... and regress
old.lm = lm(y ~ x.z[,1])
new.lm = lm(new.y ~ new.x.z[,1])
abline(old.lm$coefficients)
abline(new.lm$coefficients,col="blue")

# Figure 5
x <- runif(100)
y <- rnorm(100,mean=log(x),sd=1)
plot(y~x)
curve(log(x),add=TRUE,col="grey")
abline(lm(y~x))

