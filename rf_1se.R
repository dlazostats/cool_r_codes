#Random forest
#------------
library(dplyr)
library(caret)
library(ranger)
              
# set working directory
script_name <- 'rf_1se.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
dt0<-read.csv("train_qtb.csv")
dt1<-dt0 %>% dplyr::select(FLUENCIA,PE_MET:material_c)

#train/test
indx<-createDataPartition(dt1$FLUENCIA,list=F)
train<-dt1[indx,]
test<-dt1[-indx,]

#train model
# oneSE
#--------
set.seed(1212)
ctrl1 <- trainControl(method = "cv",
                     number = 10,
                     selectionFunction = "oneSE")
tune_grid1 <- expand.grid(.mtry = c(3, 4, 5,10),
                         .splitrule = "variance", 
                         .min.node.size = c(3, 5, 10))
rf_mdl1<-train(FLUENCIA~.,
              data=train,
              method = "ranger",
              tuneGrid = tune_grid1,
              trControl = ctrl1)
rf_mdl1$results[1,]
rf_mdl1$bestTune
densityplot(rf_mdl1)
param_1se<-rf_mdl1$bestTune

# min rmse
#----------
set.seed(1212)
ctrl2 <- trainControl(method = "cv",
                     number = 10)
tune_grid2 <- expand.grid(.mtry = c(3, 4, 5,10),
                         .splitrule = "variance", 
                         .min.node.size = c(3, 5, 10))
rf_mdl2<-train(FLUENCIA~.,
              data=train,
              method = "ranger",
              tuneGrid = tune_grid2,
              trControl = ctrl2)
rf_mdl2$results[which.min(rf_mdl2$results$RMSE),]
rf_mdl2$bestTune
densityplot(rf_mdl2)
param_bst<-rf_mdl2$bestTune