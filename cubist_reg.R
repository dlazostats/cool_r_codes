# Cubist Regression and Random Forest
#------------------------------------
library(randomForest)
library(mlbench)
library(Cubist)
library(MLmetrics)

data("BostonHousing")
House = BostonHousing[, -14]
value = BostonHousing$medv

mod1  = cubist(x = House, y = value)
pred1 = predict(mod1,newdata=House)
RMSE(pred1,value)

mod4  = cubist(x = House, y = value, committees = 4)
pred4 = predict(mod4,newdata=House)
RMSE(pred4,value)

mod10 = cubist(x = House, y = value, committees = 10)
pred10 = predict(mod10,newdata=House)
RMSE(pred10,value)

res<-numeric()
for(i in 1:10){
  mod = cubist(x = House, y = value, committees = i)
  pred = predict(mod,newdata=House)
  mtrc<-RMSE(pred,value)
  res[[i]]<-mtrc
}
plot(res,type="l")
dr<-data.frame(comm=1:10,mtrc=res)


plot(value, pred1, col = "black")
points(value, pred4, col = "red")

plot(value, pred1, col = "black")
points(value, pred10, col = "blue")

#Comparison with RandomForest
rf = randomForest(medv ~ ., data = BostonHousing)
rf
pred_rf<-predict(rf)
RMSE(pred_rf,value)

library(pdp)

# Partial dependence for one variable
pdp_plot <- partial(mod4, pred.var = "lstat", train = House)
plotPartial(pdp_plot)
