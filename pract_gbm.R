#Practice ML
#------------
library(dplyr)
library(rpart)
library(caret)
library(tidymodels)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)

# split into train/test
set.seed(2121)
splitt<-initial_split(db1,prop=0.8,strata=RES_TRA)
train<-training(splitt)
test<-testing(splitt)
combined <- bind_rows(
  train %>% mutate(set = "train"),
  test  %>% mutate(set = "test")
)
ggplot(combined, aes(x = RES_TRA, fill = set, color = set)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  theme_minimal() +
  labs(title = "Target density: train vs test", x = "target")

# GBM
set.seed(42)
fit <- train(
  RES_TRA ~ .,          
  data = train,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  verbose = FALSE       # passed through to gbm, suppresses per-tree output
)
fit
fit$finalModel
fit$bestTune 
densityplot(fit)
varImp(fit) 

# hyperparameter tunning
grid <- expand.grid(
  n.trees           = seq(100, 1000, by = 100),  # boosting iterations
  interaction.depth = c(2, 3, 5, 8),                # tree depth / interaction order
  shrinkage         = c(0.01, 0.1),              # learning rate
  n.minobsinnode    = c(10, 20)                  # min obs per terminal node
)

ctrl <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3)

fit <- train(RES_TRA ~ ., 
             data = train,
             method = "gbm",
             trControl = ctrl,
             tuneGrid = grid,
             metric = "RMSE",          # or "Accuracy", "RMSE", "Kappa"
             verbose = FALSE)
fit
fit$finalModel
fit$bestTune 
densityplot(fit)
