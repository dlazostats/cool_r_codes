#Practice ML
#------------
library(dplyr)
library(rpart)
library(mboost)
library(rpart.plot)
library(ranger)
library(dbarts)
library(caret)
library(dbarts)
library(bartMachine)
library(partykit)
library(party)
library(parttree)
library(tidymodels)
library(MLmetrics)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)