library(dplyr)
library(MLmetrics)
library(rstanarm)
library(caret)
library(elasticnet) 

#### Diabetes example
setwd("C:/Users/DiegoAlonsoLazoPaz/Downloads")
load("diabetes.RData") 
yf<-diabetes$y
yf<-(yf-mean(yf))/sd(yf)

Xf<-diabetes$X 
Xf<-t( (t(Xf)-apply(Xf,2,mean))/apply(Xf,2,sd))

## set up training and test data
n<-length(yf)
set.seed(1)

i.te<-sample(1:n,100)
i.tr<-(1:n)[-i.te]

y<-yf[i.tr] ; y.te<-yf[i.te]
X<-Xf[i.tr,]; X.te<-Xf[i.te,]
dtr<-data.frame(cbind(y,X))
dtst<-data.frame(cbind(y.te,X.te))

# linear regression (Ordinary Least Squares)
lm1<-lm(y~-1+X)
y_ols<-predict(lm1,newdata = list(X = X.te))
plot(y.te,y_ols)
RMSE(y_ols,y.te)
MSE(y_ols,y.te)

# bayesian regression
p      <- ncol(X)
n      <- nrow(X)
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% y
s2_ols   <- sum((y - X %*% beta_ols)^2) / (n - p)
blm1 <- stan_glm(y ~ -1 + .,
                 data     = dtr,
                 family   = gaussian(),
                 prior    = normal(0, sqrt(n * s2_ols)),  # unit info prior scale
                 prior_aux = exponential(1),
                 chains   = 4,
                 iter     = 2000,
                 seed     = 1)
y_bayes <- predict(blm1, newdata = dtst)
plot(y.te,y_bayes)
RMSE(y_bayes,y.te)
MSE(y_bayes,y.te)

# Lasso
train_control <- trainControl(
  method = "cv",        # k-fold cross-validation
  number = 10,          # number of folds
  selectionFunction = "oneSE"  # use 1-SE rule for lambda selection
)
tune_grid <- expand.grid(
  fraction = seq(0.01, 1, length = 50)
)
l_reg<-train(y~-1+.,
             data=dtr,
             method="lasso",
             trControl = train_control,
             preProcess = c("center", "scale"),
             tuneGrid=tune_grid)
l_reg
densityplot(l_reg)
y_lasso<-predict(l_reg,dtst)
plot(y.te,y_lasso)
RMSE(y_lasso,y.te)
MSE(y_lasso,y.te)

# Ridge
tune_grid <- expand.grid(
  alpha  = 0,                            # 0 = Ridge, 1 = LASSO, in-between = Elastic Net
  lambda = 10^seq(-4, 2, length = 100)   # range of penalty values to search
)
l_ridg<-train(y~-1+.,
             data=dtr,
             method="glmnet",
             trControl = train_control,
             preProcess = c("center", "scale"),
             tuneGrid=tune_grid)
l_ridg
densityplot(l_ridg)
y_rid<-predict(l_ridg,dtst)
plot(y.te,y_rid)
RMSE(y_rid,y.te)
MSE(y_rid,y.te)

