# Nested CV
#-----------
library(dplyr)
library(caret)
library(nestedcv)

# setwd
script_name <- 'nested_cv.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
dta0<-read.csv("dtf_test.csv")

# train/test  
indx<-createDataPartition(dta0$FLUENCIA,p=0.8,list=F)  
train <- dta0[indx,]
test <- dta0[-indx,]
xtrain<-train %>% select(-FLUENCIA)
ytrain<-train %>% select(FLUENCIA)

# CV normal
fitcontrol<-trainControl(method="cv",number=10)
glm_cv<-train(FLUENCIA~.,
              train,
              method="glmnet",
              tunelength=10,
              trControl=fitcontrol)
sapply(list(glm_cv$results$RMSE,glm_cv$results$Rsquared,glm_cv$results$MAE),mean)

# nested CV using caret
glm_nestcv<-nestcv.train(ytrain$FLUENCIA,
                         xtrain,
                         method="glmnet",
                         outer_method = c("cv"),
                         n_outer_folds = 10,
                         n_inner_folds = 10,
                         tunelength=10,
                         cv.cores=2)
glm_nestcv$summary
plot(glm_nestcv$outer_result[[1]]$fit, xTrans = log) # plot outer fold
plot(innercv_preds(glm_nestcv))
