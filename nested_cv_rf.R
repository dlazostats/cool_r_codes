## Nested CV- uantile Regression
#-----------------------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)
library(summarytools)
library(caret)

# set working directory
script_name <- 'nested_cv_rf.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0[sample(1:nrow(db0),1500),] %>% dplyr::select(Fluencia,PesoMetrico:Pb,material_c)

# quick EDA
hist(db0_sqmple$Fluencia,breaks = "FD")
descr(db0_sqmple)

# Using Caret
#----------------------------------------------------------------------------------------------------------------------
outer_folds <- createFolds(db0_sqmple$Fluencia, k = 5, returnTrain = TRUE)
outer_scores <- numeric(length(outer_folds))

for (i in seq_along(outer_folds)) {
  
  # --- split into outer train / outer test ---
  train_idx <- outer_folds[[i]]
  test_idx  <- setdiff(seq_len(nrow(db0_sqmple)), train_idx)
  
  outer_train <- db0_sqmple[train_idx, ]
  outer_test  <- db0_sqmple[test_idx,]
  
  # --- inner loop: 3-fold CV for hyperparameter tuning ---
  inner_ctrl <- trainControl(
    method  = "cv",
    number  = 5,
    verboseIter = TRUE,
    selectionFunction = "oneSE"
  )
  tune_grid <- expand.grid(
    mtry            = c(3, 4, 5, 6),
    splitrule       = c("variance", "maxstat"),
    min.node.size   = c(3, 5)
  )
  best_model  <- train(Fluencia ~ .,
                       data      = outer_train,
                       method    = "ranger",
                       trControl = inner_ctrl,
                       tuneGrid  = tune_grid,
                       metric    = "RMSE",      
                       num.trees = 500,
                       importance = "permutation")
  
  # --- evaluate on outer test fold ---
  preds <- predict(best_model, outer_test)
  outer_scores[i] <- RMSE(preds,outer_test$Fluencia)
  
  cat(sprintf("Outer fold %d | best C=%.1f sigma=%.2f | acc=%.3f\n",
              i,
              best_model$bestTune$C,
              best_model$bestTune$sigma,
              outer_scores[i]))
  
}


