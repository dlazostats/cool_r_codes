# BART  - Compelte Guide
#=======================
library(dplyr)
library(tidymodels)
library(caret)
library(dbarts)
library(ppsr)
library(bartMachine)
library(SoftBart)
library(MASS)

## Working directory
script_name <- 'bart_final_guide.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

#Loading data and assesing predictive power
ppsr::score_predictors(df = Boston, y = 'medv') %>% arrange(desc(pps))
dbml<-Boston %>% dplyr::select(-chas)

# Train/test set
set.seed(2134)
split<-initial_split(dbml,prop=0.8,strata=medv)
train<-training(split)
test<-testing(split)
dim(train);dim(test)

# ML Models
#-----------
## Random Forest
#-----------------
set.seed(2131)
fitcontrol<-trainControl(method="repeatedcv",repeats = 3,number=3)
rf_ml<-train(medv~.,
             data=train,
             method="ranger",
             trControl=fitcontrol)
#train error
mean(rf_ml$resample$RMSE)
mean(rf_ml$resample$Rsquared)

## test error
p_rf<-predict(rf_ml,test)   
postResample(p_rf,test$medv)

## BART
## caret / bartmachine
set.seed(3123)
bartGrid <- expand.grid(num_trees = c(150,200),beta = c(2,3), 
                        k = c(2,3),alpha=c(0.90,0.95), nu = 3)
bart_caret<-train(medv ~.,
                  data=train,
                  method="bartMachine",
                  tuneGrid = bartGrid,
                  trControl=fitcontrol)
bart_caret$bestTune

#train error
mean(bart_caret$resample$RMSE)
mean(bart_caret$resample$Rsquared)

## test error
p_b1<-predict(bart_caret,test)   
postResample(p_b1,test$medv)

## credible intervals
set.seed(3123)
Xt<-train %>% dplyr::select(-medv)
yt<-train %>% dplyr::select(medv) %>% pull()
Xtest<-test %>% dplyr::select(-medv)
ytest<-test %>% dplyr::select(medv) %>% pull()

bartmch<-bartMachine(Xt,yt,num_trees=150,k=3,alpha=0.9,beta=2,nu=3)
posterior = bart_machine_get_posterior(bartmch, Xt)
oos_perf = bart_predict_for_test_data(bartmch, Xtest, ytest)
print(oos_perf$rmse)
cred_int = calc_credible_intervals(bartmch, Xtest)
pred_int = calc_prediction_intervals(bartmch, Xtest)
head(cred_int)
print(head(pred_int$interval))
p_hat=predict(bartmch,Xtest)
cred_int_df<-cred_int %>% as.data.frame() %>% mutate(pred=p_hat)
cred_int_df<-cred_int_df[,c(1,3,2)] %>% mutate(real=test$medv) %>% 
             mutate(cov=ifelse(real>=ci_lower_bd|real<=ci_upper_bd,1,0)) %>% 
             mutate(dif_i=ci_upper_bd-ci_lower_bd)
RMSE(p_hat,test$medv)

plot(test$medv,type="l")
lines(cred_int_df$pred,col="blue")
lines(cred_int_df$ci_upper_bd,col="red")
lines(cred_int_df$ci_lower_bd,col="red")

## DBARTS
bart_db<-bart2(medv~.,data=train,keepTrees = T) # T for forecasting

## train error
RMSE(bart_db$yhat.train.mean,train$medv)

## Test error
yht_dist<-predict(bart_db,test,type='ppd') 
yht_pt<-apply(yht_dist,2,mean)
RMSE(yht_pt,test$medv)
means<-apply(yht_dist,MARGIN = 2,mean)
lci<-apply(yht_dist,MARGIN = 2,
           function(x){quantile(x,0.025)})
uci<-apply(yht_dist,MARGIN = 2,
           function(x){quantile(x,0.975)})
drf<-data.frame(lci,means,uci) %>% mutate(real=test$medv) %>% 
      mutate(cov=ifelse(real>=uci|real<=uci,1,0)) %>% 
      mutate(dif_i=drf$uci-drf$lci)

## SoftBart
# A) softbart_regression
softbart_m <- softbart_regression(medv ~ .,
                                  data = train,
                                  test_data = test)
plot(softbart_m)
plot(colMeans(softbart_m$mu_test), test$medv)
abline(a = 0, b = 1)
yhat_sof <- predict(softbart_m, test)
RMSE(yhat_sof$mu_mean,test$medv)
MAE(yhat_test_sof$mu_mean,test$fluenc)

# B) softbart
softbart2<-softbart(Xt,yt,Xtest)  # GOOD
plot(softbart2)
RMSE(softbart2$y_hat_train_mean,train$medv)
RMSE(softbart2$y_hat_test_mean,test$medv)
quantile(softbart2$y_hat_test,c(0.025,0.975))

lci_sf2<-apply(softbart2$y_hat_test,MARGIN = 2,function(x){quantile(x,0.025)})
uci_sf2<-apply(softbart2$y_hat_test,MARGIN = 2,function(x){quantile(x,0.975)})
promsf2<-softbart2$y_hat_test_mean
dr_sof2<-data.frame(low=lci_sf2,mean=promsf2,up=uci_sf2)
dr_sof2$difi_i<-dr_sof2$up-dr_sof2$low


hypers <- Hypers(X = Xt, Y = yt,
                 num_tree = 30, beta = 2, gamma = 3,k=2)
sbart_m<-softbart(Xt,yt,Xtest,hypers=hypers)


