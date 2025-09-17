# GLM linear regression
#----------------------
library(dplyr)
library(dobson)
library(jtools)
library(ggeffects)
library(performance)

data("carbohydrate")
dt0<-carbohydrate
dt00<-dt0 %>% mutate(interp=1)
head(dt0)

# lm results
mdl_lm<-lm(carbohydrate~.,data=dt0)
summary(mdl_lm)
summ(mdl_lm)
ggpredict(mdl_lm, terms = "protein") %>% plot()
check_model(mdl_lm)

# Matrix manipulation
y<-dt0$carbohydrate
X<-as.matrix(dt00[,c(5,2:4)])

t(X)%*%y
t(X)%*%X
solve(t(X)%*%X)

#therefor
beta<-solve(t(X)%*%X)%*%t(X)%*%y
beta
coef(mdl_lm)
e<-y-X%*%beta
var_mdl<-(t(e)%*%e)/(nrow(X)-ncol(X))
sd_mdl<-sqrt(var_mdl)
