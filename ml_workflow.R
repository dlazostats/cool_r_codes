# Libraries
library(dplyr)
library(tidymodels)

# set working directory
script_name <- 'ml_workflow.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
dt0<-read.csv("train_qtb.csv")
dt1<-dt0 %>% dplyr::select(FLUENCIA,PE_MET:material_c)

# train/test/val set
split<-initial_split(dt1,strata=FLUENCIA)
train<-training(split)
test<-testing(split)

# workflow
kcv <- vfold_cv(train,v=4)


