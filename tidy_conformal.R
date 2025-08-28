# Conformal Tidymodels
#----------------------
library(dplyr)
library(tidymodels)
library(probably)

#set working directory
setwd("C:/Users/dlazo/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/Diego/cool_r_codes")

# load data
dt0<-read.csv("train_qtb.csv")
dt1<-dt0 %>% select(RELACION,PE_MET:material_c)

# data split
n<-500
set.seed(1211)
dt1s<-dt1[sample(1:nrow(dt1),n),]
split<-initial_validation_split(dt1s,strata=RELACION)
train<-training(split)
test<-testing(split)
calib<-validation(split)

# tidymodel and conformal
#-------------------------------------
cv_folds<-vfold_cv(train,v=10,strata=RELACION)

svm_spec<-svm_rbf() %>% 
          set_mode("regression")
svm_flow<-workflow(RELACION~.,svm_spec)
svm_fit<-svm_flow %>% fit(data=train)

conf_split<-int_conformal_split(svm_fit,cal_data=calib)
conf_split_test<-predict(conf_split,test,level=0.9) %>% 
                 bind_cols(test) %>% 
                 mutate(cov=ifelse(RELACION<=.pred_upper & RELACION>=.pred_lower,1,0))
conf_split_test 
mean(conf_split_test$cov)

plot(test$RELACION,type="l",ylim=c(1.1,1.4))
lines(conf_split_test$.pred_lower,col="red")
lines(conf_split_test$.pred_upper,col="red")

#cv+inference (when don't have enough data use CV)
#-------------------------------------
ctrl<-control_resamples(save_pred=T,extract=I)
svm_resamp<-svm_flow %>% 
            fit_resamples(resamples = cv_folds,control=ctrl)
conf_cv<-int_conformal_cv(svm_resamp)
conf_cv_test<-predict(conf_cv,test,level=0.9) %>% 
              bind_cols(test)%>% 
              mutate(cov=ifelse(RELACION<=.pred_upper & RELACION>=.pred_lower,1,0))
mean(conf_cv_test$cov)

# Conformalized Quantile Regression
#-------------------------------------
set.seed(8181)
conf_qntl<-int_conformal_quantile(svm_fit,
                                  train_data = train,
                                  cal_data = calib,
                                  level=0.9,
                                  ntree=2000)
conf_qntl_test<-predict(conf_qntl,test) %>% 
                bind_cols(test) %>% 
                mutate(cov=ifelse(RELACION<=.pred_upper & RELACION>=.pred_lower,1,0))
mean(conf_qntl_test$cov)

plot(test$RELACION,type="l",ylim=c(1.1,1.4))
lines(conf_qntl_test$.pred_lower,col="red")
lines(conf_qntl_test$.pred_upper,col="red")




