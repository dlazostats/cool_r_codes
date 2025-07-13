## Softbar
library(dplyr)
library(caret)
library(SoftBart)
library(MLmetrics)

data("bodyfat", package = "TH.data")
set.seed(3456)
trainIndex <- createDataPartition(bodyfat$DEXfat, p = .8,list=F)
train <- bodyfat[trainIndex,]
test  <- bodyfat[-trainIndex,]

# Models
fitted_regression <- softbart_regression(DEXfat ~ .,
                                         data = train,
                                         test_data = test)
predicted_values <- predict(fitted_regression, test)
predicted_values
fitted_regression
rmse(test$DEXfat,predicted_values$mu_mean)
plot(test$DEXfat,type="l")
lines(predicted_values$mu_mean,col="red")

variable_selection <- posterior_probs(fitted_regression)
variable_selection$varimp[order(variable_selection$varimp,decreasing = T)]
plot(variable_selection$post_probs)
print(variable_selection$median_probability_model)
fitted_regression$sigma_mu %>% length()
dim(train)
dim(test)
lapply(predicted_values,print)

  