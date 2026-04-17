## Nested CV- uantile Regression
#-----------------------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)
library(summarytools)
library(tidymodels)
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
    number  = 3,
    #verboseIter = TRUE,
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
  preds <- predict(best_model, outer_test,OOB = TRUE)
  outer_scores[i] <- RMSE(preds,outer_test$Fluencia)
  
  print(paste0("Outer fold ", i, 
               " | best mtrt = ",best_model$bestTune$mtry,
               " | best splitrule = ", best_model$bestTune$splitrule,
               " | best min.node.size = ", best_model$bestTune$min.node.size,
              " | RMSE = ",round(outer_scores[i],2)))
}
cat(sprintf("\nNested CV accuracy: %.3f ± %.3f\n",
            mean(outer_scores),
            sd(outer_scores)))

final_ctrl <- trainControl(
  method            = "cv",
  number            = 3,
  selectionFunction = "best"
)
Final_model  <- train(Fluencia ~ .,
                      data      = db0_sqmple,
                      method    = "ranger",
                      trControl = final_ctrl,
                      tuneGrid  = tune_grid,
                      metric    = "RMSE",      
                      num.trees = 500,
                      importance = "permutation")
Final_model$finalModel
Final_model$bestTune
densityplot(Final_model)

# Now using tidymodels
#---------------------
nested_rs <- nested_cv(
  db0_sqmple,
  outside = vfold_cv(v = 5),
  inside  = vfold_cv(v = 3)
)
nested_rs






set.seed(123)  # for reproducibility

# Example data
df <- data.frame(x = mtcars$mpg)

# Choose 30% of rows randomly
idx <- sample(1:nrow(df), size = 0.3 * nrow(df))

# Replace those values with 0
df$x[idx] <- 0

df
mean(df$x)
mean(df$x[df$x>0])
median(df$x)
median(df$x[df$x>0])


























