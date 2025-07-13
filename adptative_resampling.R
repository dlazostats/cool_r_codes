#adaptative resampling
#----------------------
library(caret)
library(dplyr)
library(tidymodels)
library(dbarts)
library(ppsr)
library(bartMachine)
library(MASS)

#Loading data and assesing predictive power
ppsr::score_predictors(df = Boston, y = 'medv') %>% arrange(desc(pps))
dbml<-Boston %>% dplyr::select(-chas)

# Train/test set
set.seed(2134)
split<-initial_split(dbml,prop=0.8,strata=medv)
train<-training(split)
test<-testing(split)
dim(train);dim(test)

## Random Forest
# Random repeated CV
set.seed(1212)
fitControl<-trainControl(method="repeatedcv",
                         repeats = 3,number=3)
gbm_model_repeated <- train(medv ~ .,
                            data = train,
                            method ="gbm",
                            trControl = fitControl,
                            verbose =FALSE,
                            tuneLength=10)

#train error
mean(gbm_model_repeated$resample$RMSE)
mean(gbm_model_repeated$resample$Rsquared)

## test error
p_gbm_rep<-predict(gbm_model_repeated,test)   
postResample(p_gbm_rep,test$medv)

# adpatative resampling
set.seed(1212)
fitControl<-trainControl(method="adaptive_cv",
                         adaptive=list(min=2,alpha=0.05,
                                         method="gls",
                                         complete=TRUE),
                         search = "random")
gbm_model_resampl <- train(medv ~ .,
                           data = train,
                           method ="gbm",
                           trControl = fitControl,
                           verbose =FALSE,
                           tuneLength=10)
gbm_model_resampl

#train error
mean(gbm_model_resampl$resample$RMSE)
mean(gbm_model_resampl$resample$Rsquared)

## test error
p_gbm_rep_ad<-predict(gbm_model_resampl,test)   
postResample(p_gbm_rep_ad,test$medv)
