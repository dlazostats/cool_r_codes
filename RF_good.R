tune_grid <- expand.grid(
  mtry = c(2:18),
  splitrule = "variance",
  min.node.size = c(5,10,20)
)
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

# Train model with tuning
library(caret)
set.seed(42)
rf_model <- train(RES_TRA ~ .,
                  data = train,
                  method = "ranger",
                  trControl = ctrl,
                  tuneGrid = tune_grid,
                  num.trees = 500,
                  importance = 'impurity')
rf_model$resample
print(rf_model$bestTune)
densityplot(rf_model, pch = "|")
ggplot(rf_model$results, aes(x = factor(mtry), y = RMSE)) +
  geom_boxplot(aes(group = mtry)) +
  labs(title = "RMSE by mtry value", x = "mtry", y = "RMSE") +
  theme_minimal()

ggplot(rf_model$results, aes(x = factor(mtry), y = RMSE, color = factor(min.node.size))) +
  geom_point(size = 3) +
  geom_line(aes(group = factor(min.node.size))) +
  labs(title = "Train RMSE by mtry and min.node.size",
       x = "mtry", color = "min.node.size") +
  theme_minimal()
