## Quantile Regression
#-----------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)

data(airquality)
set.seed(1)
airquality <- airquality[ !apply(is.na(airquality), 1,any), ]
n <- nrow(airquality)
hist(airquality$Ozone,breaks="FD")

## divide into training and test data
indextrain <- sample(1:n,round(0.6*n),replace=FALSE)
Xtrain     <- airquality[ indextrain,2:6]
Xtest      <- airquality[-indextrain,2:6]
Ytrain     <- airquality[ indextrain,1]
Ytest      <- airquality[-indextrain,1]

qrf <- quantregForest(x=Xtrain, y=Ytrain,keep.inbag=TRUE )
qrf
plot(qrf)

## compute out-of-bag predictions 
quant.outofbag <- predict(qrf)

## predict test data
quant.newdata  <- predict(qrf, newdata= Xtest)
res_qtlpred <-quant.newdata %>% as.data.frame()

## estimate conditional standard deviation
conditionalSd <- predict(qrf,  Xtest, what=sd)

## estimate conditional mean (as in original RF)
conditionalMean <- predict(qrf,  Xtest, what=mean)

## Results
res_pred<-cbind(data.frame(Ozone=Ytest),Xtest) %>% 
          mutate(condmed=conditionalMean)

## plot results 
plot(Ytest,type="p")
lines(conditionalMean,col="red")            
lines(Ytest,col="grey")
lines(res_qtlpred$`quantile= 0.1`,col="blue")
lines(res_qtlpred$`quantile= 0.9`,col="blue")

plot(Ytest,type="p")
lines(conditionalMean,col="red")            
lines(res_qtlpred$`quantile= 0.1`,col="blue")
lines(res_qtlpred$`quantile= 0.9`,col="blue")

## get ecdf-function for each new test data point
## (output will be a list with one element per sample)
condEcdf <- predict(qrf,  Xtest, what=ecdf)
condEcdf[[10]](30) ## get the conditional distribution at value 30 for i=10
plot(condEcdf[[1]])

## conditional distribution
# a) with predict
probs <- seq(0.01, 0.99, by = 0.01)
preds <- predict(qrf, newdata = data.frame(Temp = 80, Wind = 10, Solar.R = 200,Month=5,Day=15),
                 what = probs)
preds <- predict(qrf, newdata = Xtest[1,], what = probs)
plot(preds, probs, type = "l", xlab = "Ozone", ylab = "Cumulative probability")

plot(condEcdf[[1]])
plot(preds, probs, type = "l", xlab = "Ozone", ylab = "Cumulative probability")

class(qrf) <- "randomForest"
importance(qrf)

## ML metrics
vv<-as.numeric(preds)
hist(vv,breaks="FD")

