## Bayesian Additive Regression trees
#====================================
library(ggplot2)
library(dplyr)
library(bartMachine)
library(dbarts)
library(tidymodels)
library(GGally)
library(hrbrthemes)
library(summarytools)
library(caret)
library(patchwork)

# Function
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

## Working directory
script_name <- 'bart1.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)
source("plot_error.R")

## Load data
db0<-read.csv("physics.csv")

## EDA
### Statistics
descr(db0 %>% select(-w,-z))
db0 %>% select(-w) %>% stby(INDICES = db0$z,FUN = descr,stats="common")
db0 %>% select(-z) %>% stby(INDICES = db0$w,FUN = descr,stats="common")

### Plots
db0$w<-as.factor(db0$w)
db0$z<-as.factor(db0$z)
ggpairs(db0)
ggpairs(db0 %>% select(-z),mapping=aes(color=w))
ggpairs(db0%>% select(-w),mapping=aes(color=z))
ggpairs(db0[,c(1,2,5)], 
        lower = list(continuous = wrap(lowerFn, method = "lm")),
        diag = list(continuous = wrap("barDiag", colour = "blue")),
        upper = list(continuous = wrap("cor", size = 10)))

## ML model
### Train/test
set.seed(1123)
split<-initial_split(db0,prop=0.8,strata = y)
train<-training(split)
test<-testing(split)
dim(train);dim(test)
par(mfrow=c(1,2))
hist(train$y)
hist(test$y)
fitControl <- trainControl(method = "repeatedcv",
                           repeats=3,number=3)
## Linear regresion
m_lm<-train(y~.,
            data=train,
            method="lm",
            trControl=fitControl)
yht_lm_tn<-predict(m_lm)
yht_lm_test<-predict(m_lm,test)
RMSE(yht_lm_tn,train$y)
RMSE(yht_lm_test,test$y)
plot_error(yht_lm_test,test$y)

## GBM
gbm_m<-train(y ~ .,
             data=train,
             method="gbm",
             trControl=fitControl,
             tuneLength=10)
yht_gbm_tn<-predict(gbm_m)
yht_gbm_test<-predict(gbm_m,test)
RMSE(yht_gbm_tn,train$y)
RMSE(yht_gbm_test,test$y)
plot_error(yht_gbm_test,test$y)

## BART
bart_m<-bart2(y~.,data=train,keepTrees = T) # for forecasting
yht_bart_tn<-predict(bart_m)
yht_bart_test<-predict(bart_m,test,type='ppd') ##ppd draws from posterior 
dim(yht_bart_test)
dim(test)

## point prediction
point<-apply(yht_bart_test,2,mean)
RMSE(point,test$y)

## prediction interval for one test set point
hist(yht_bart_test[,10],main="Prediction for obs 10",xlab="xmetric")
quantile(yht_bart_test[,10],c(0.025,0.975)) #95% credible interval

# tunning k with xbart
cv<-xbart(y~.,data=train,
          k=seq(1,5,0.1),seed=2) # 1 to 5
cv_mean<-apply(cv,2,mean)
cv_mean[cv_mean==min(cv_mean)]
kval<-as.numeric(names(cv_mean[cv_mean==min(cv_mean)]))

bart_mcv<-bart2(y~.,data=train,keepTrees = T,
                k=kval,verbose = F)
yht_bart_test2<-predict(bart_mcv,test,type='ppd') ##ppd draws from posterior 
point2<-apply(yht_bart_test2,2,mean)
RMSE(point2,test$y)

means<-apply(yht_bart_test2,MARGIN = 2,mean)
lci<-apply(yht_bart_test2,MARGIN = 2,
           function(x){quantile(x,0.025)})
uci<-apply(yht_bart_test2,MARGIN = 2,
           function(x){quantile(x,0.975)})
drf<-data.frame(lci,means,uci)

plot(test$y,type="l")
lines(drf$means,col="blue")
lines(drf$lci,col="red",type="l")
lines(drf$uci,col="red")

drf$real<-test$y
drf$int_cov<-ifelse(drf$real>=drf$lci & drf$real<=drf$uci,1,0)
summary(drf)
