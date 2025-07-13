# Poisson regression
library(dobson)
library(dplyr)

# Example 1
data(poisson)
res.p=glm(y~x,family=poisson(link="identity"),data=poisson)
summary(res.p)
dta<-poisson %>% mutate(pred=predict(res.p))
plot(poisson$x,poisson$y)
points(poisson$x,dta$pred,col="red",pch=16)

# Example 2
dtaid<-data.frame(x=seq(1:14),
                  y=c(0,1,2,3,1,4,9,18,23,31,20,25,37,45))
plot(dtaid$x,dtaid$y,pch=16)
abline(lm(y~x,data=dtaid)) # linear model
lm_m<-lm(y~x,data=dtaid)

# log mes
plot(dtaid$x,log1p(dtaid$y),pch=16)
abline(lm(log1p(y)~x,data=dtaid)) # log-linear model
lm(log1p(y)~x,data=dtaid)
