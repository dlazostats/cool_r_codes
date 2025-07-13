library(ngboost)
library(MLmetrics)
data(Boston, package = "MASS")
dta <- rsample::initial_split(Boston)
train <- rsample::training(dta)
test <- rsample::testing(dta)


x_train = train[,1:13]
y_train = train[,14]

x_test = test[,1:13]
y_test = test[,14]

model <- NGBRegression$new(Dist = Dist("Exponential"),
                           Base = sklearner(),
                           Score = Scores("MLE"),
                           natural_gradient =TRUE,
                           n_estimators = 600,
                           learning_rate = 0.002,
                           minibatch_frac = 0.8,
                           col_sample = 0.9,
                           verbose = TRUE,
                           verbose_eval = 100,
                           tol = 1e-5)
model$fit(X = x_train, Y = y_train, X_val = x_test, Y_val = y_test)
model$feature_importances()
model$plot_feature_importance()

model$predict(x_test)%>%head()
RMSE(model$predict(x_test),test$medv)
distt <- model$pred_dist(x_test) # it returns a NGBDist
drp<-data.frame(li=distt$interval(confidence = .9)[[1]],
                med=model$predict(x_test),
                ls=distt$interval(confidence = .9)[[2]])  

plot(test$medv,type="l")
lines(drp$li,col="red")
lines(drp$med,col="blue")
lines(drp$ls,col="red")
