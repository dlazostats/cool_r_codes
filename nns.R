library(NNS)
library(ggplot2)
set.seed(159951)
x_train<-runif(200,-2,1)
y_train<-sin(pi*x_train)+rnorm(200,sd=0.2)
x_test<-seq(-2,2,length.out=100)
NNS.reg(x=data.frame(x=x_train),y=y_train,order=NULL,residual.plot = TRUE)
NNS.reg(x_train, y_train, smooth = T)

mdlnnss<-NNS.reg(x=data.frame(x=x_train),y=y_train,order=NULL,plot=F)
predict(mdlnnss,newdata=x_test)

x <- cbind(rnorm(100), rnorm(100), rnorm(100)) ; y <- rnorm(100)
NNS.reg(x, y, point.est = c(.25, .5, .75))

NNS.reg(x=data.frame(x=x_train),y=y_train,order=NULL,point.est = x_test)

plot(mdlnnss$Fitted.xy$x.x,mdlnnss$Fitted.xy$y.hat,type="l")
dtf<-data.frame(x=mdlnnss$Fitted.xy$x.x,
                y=mdlnnss$Fitted.xy$y.hat)
ggplot(dtf,aes(x=x,y=y))+
  geom_line()+
  geom_point()
NNS.reg(x=data.frame(x=x_train),y=y_train,order=NULL,residual.plot = TRUE)

