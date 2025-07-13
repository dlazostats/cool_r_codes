## Bayesian Additive Regression trees
#====================================
library(ggplot2)
library(dplyr)
library(bartMachine)
library(dbarts)
library(tidymodels)
library(GGally)
library(SoftBart)
library(hrbrthemes)
library(summarytools)
library(caret)
library(patchwork)
library(MLmetrics)

# Function
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

## Working directory
script_name <- 'bart2_qtbtemp.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)
source("plot_error.R")

## Load data
db0<-read.csv("datatemp_clean.csv")
db1<-db0 %>% select(-rest_trac,-alarg)

## EDA
### Statistics
descr(db1)
db1 %>% stby(INDICES = db0$dim_prom,
             FUN = descr,
             stats=c("mean","sd"),transpose=T)

## ML model
### Train/test
set.seed(1123)
split<-initial_split(db1,prop=0.8,strata = fluenc)
train<-training(split)
test<-testing(split)
dim(train);dim(test)

## BART
bart_m<-bart2(fluenc~.,
              data=train,
              keepTrees = T) 

## Metrics
## Train
RMSE(bart_m$yhat.train.mean,train$fluenc)
MAE(bart_m$yhat.train.mean,train$fluenc)

## Test
yht_testdist<-predict(bart_m,test,type='ppd') ##ppd draws from posterior 
yht_test<-apply(yht_testdist,2,mean)
RMSE(yht_test,test$fluenc)
MAE(yht_test,test$fluenc)

## Test credible intervals
## 95% credible
means<-apply(yht_testdist,MARGIN = 2,mean)
lci<-apply(yht_testdist,MARGIN = 2,
           function(x){quantile(x,0.025)})
uci<-apply(yht_testdist,MARGIN = 2,
           function(x){quantile(x,0.975)})
prd_test<-data.frame(lci,means,uci)
prd_test$int_size<-prd_test$uci-prd_test$lci
prd_test$Real<-test$fluenc
prd_test$int_cov<-ifelse(prd_test$Real>=prd_test$lci & prd_test$Real<=prd_test$uci,1,0)
prd_test<-sapply(prd_test,round,1) %>% as.data.frame()
summary(prd_test)

## 90% credible
means<-apply(yht_testdist,MARGIN = 2,mean)
lci<-apply(yht_testdist,MARGIN = 2,
           function(x){quantile(x,0.05)})
uci<-apply(yht_testdist,MARGIN = 2,
           function(x){quantile(x,0.95)})
prd_test<-data.frame(lci,means,uci)
prd_test$int_size<-prd_test$uci-prd_test$lci
prd_test$Real<-test$fluenc
prd_test$int_cov<-ifelse(prd_test$Real>=prd_test$lci & prd_test$Real<=prd_test$uci,1,0)
prd_test<-sapply(prd_test,round,1) %>% as.data.frame()
summary(prd_test)

plot(test$fluenc,type="l")
lines(prd_test$means,col="blue")
lines(prd_test$lci,col="red",type="l")
lines(prd_test$uci,col="red")

## SoftBart
softbart_m <- softbart_regression(fluenc ~ .,
                                  data = train,
                                  test_data = test)
yhat_test_sof <- predict(softbart_m, test)
RMSE(yhat_test_sof$mu_mean,test$fluenc)
MAE(yhat_test_sof$mu_mean,test$fluenc)

