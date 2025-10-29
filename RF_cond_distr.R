## regression tree and random fores conditional distribution
#-----------------------------------------------------------
library(caret)
library(dplyr)
library(rpart.plot)
library(rpart)
library(rattle)
library(MLmetrics)
options(scipen=999)

# set working directory
script_name <- 'RF_cond_distr.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
df0<-read.csv("dftrain.csv")
df1<-df0 %>% dplyr::select(TempLam,PesoMetrico,C:Pb,material_c)

## Train/test sets
set.seed(1212)
indx<-createDataPartition(df1$TempLam,p=0.8,list=FALSE)
train<-df1[indx,]
test<-df1[-indx,]

## Fit control
fitControl1 <- trainControl(method = "cv", #"repeatedcv"   
                           number = 10,   #repeats=5
                           returnResamp = "all")   
fitControl2 <- trainControl(method = "cv", #"repeatedcv"   
                           number = 10)   

# Regression tree
#-----------------
reg_tree1<-train(TempLam~.,
                 data=train,
                 method="rpart2",
                 preProcess = c('scale', 'center'),
                 trControl=fitControl2,
                 tuneLength=10)
reg_tree1
reg_tree1$bestTune
final_tree<-reg_tree1$finalModel
densityplot(reg_tree1,metric="RMSE")
plot(reg_tree1)
rpart.plot(reg_tree1$finalModel)
rpart.plot(
  final_tree,
  type = 2,           # type of plot (1 = label below split, 2 = at split)
  extra = 101,        # show fitted values and % of observations
  fallen.leaves = TRUE,
  shadow.col = "gray",# soft drop shadow
  box.palette = "GnBu",# color palette
  main = "Regression Tree for TempLam"
)
fancyRpartPlot(final_tree)

pred_train<-predict(reg_tree1,train)
pred_test<-predict(reg_tree1,test)
RMSE(pred_train,train$TempLam)
RMSE(pred_test,test$TempLam)












