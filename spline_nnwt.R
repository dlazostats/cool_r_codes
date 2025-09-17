## Spline and neural networks
#----------------------------
library(dplyr)
library(psych)
library(ggplot2)
library(tidymodels)
library(splines)

# set working directory
script_name <- 'spline_nnwt.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

#Example 1
# Load data
head(diamonds)
dim(diamonds)
set.seed(121)
dtdim<-diamonds %>% select(price,x) %>% filter(price<12000)
samp_dim<-dtdim[sample(1:nrow(dtdim),size=2500),]
pairs.panels(samp_dim)

# train/test/validation
split<-initial_validation_split(samp_dim,strata=price)
train<-training(split)
test<-testing(split)
val<-validation(split)
x_train<-train$x
x_test<-test$x
x_val<-val$x
y_train<-train$price

#Spline
#------
fit.bs <- lm(price ~ bs(x),data=train ) ## B-Spline
fit.ns <- lm(price ~ ns(x),data=train ) # natural spline
fit.sp <- smooth.spline(y_train ~ x_train) ## 

ggplot(train,aes(x=x,y=price))+
  geom_point(alpha=0.2)+
  geom_line(aes(y=predict(fit.bs)),col="red")+
  geom_line(aes(y=predict(fit.ns)),col="green")+
  geom_line(aes(y=predict(fit.sp,x=x_train)$y),col="blue")+theme_bw()

ggplot(test,aes(x=x,y=price))+
  geom_point(alpha=0.2)+
  geom_line(aes(y=predict(fit.bs,newdata = test)),col="red")+
  geom_line(aes(y=predict(fit.ns,newdata = test)),col="green")+
  geom_line(aes(y=predict(fit.sp,x=x_test)$y),col="blue")+theme_bw()

ggplot(val,aes(x=x,y=price))+
  geom_point(alpha=0.2)+
  geom_line(aes(y=predict(fit.bs,newdata = val)),col="red")+
  geom_line(aes(y=predict(fit.ns,newdata = val)),col="green")+
  geom_line(aes(y=predict(fit.sp,x=x_val)$y),col="blue")+theme_bw()

