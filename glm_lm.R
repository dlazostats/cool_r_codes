# GLM linear regression
#----------------------
library(dplyr)
library(glmnet)
library(cvTools)
library(dobson)
library(olsrr)
library(jtools)
library(ggeffects)
library(performance)

data("carbohydrate")
dt0<-carbohydrate
dt00<-dt0 %>% mutate(interp=1)
head(dt0)

# lm results
mdl_lm<-lm(carbohydrate~.,data=dt0)
summary(mdl_lm)
summ(mdl_lm)
ggpredict(mdl_lm, terms = "protein") %>% plot()
check_model(mdl_lm)

# Matrix manipulation
y<-dt0$carbohydrate
X<-as.matrix(dt00[,c(5,2:4)])

t(X)%*%y
t(X)%*%X
solve(t(X)%*%X)

#therefor
beta<-solve(t(X)%*%X)%*%t(X)%*%y
beta
coef(mdl_lm)
e<-y-X%*%beta
var_mdl<-(t(e)%*%e)/(nrow(X)-ncol(X))
sd_mdl<-sqrt(var_mdl)

# Cross- validation
res.glm=glm(carbohydrate~age+weight+protein,
            family=gaussian,data=carbohydrate)
summary(res.glm)
summ(res.glm)
plot(res.glm)

full.model <- lm(carbohydrate ~ ., data = carbohydrate)
cv_erro1<-cvFit(full.model, data = carbohydrate, K = 5, R = 100,
       y = carbohydrate$carbohydrate) # rmspe (root mean squared prediction error)

cv_erro2<-cvFit(full.model, data = carbohydrate, K = 5, R = 10,
               y = carbohydrate$carbohydrate,cost=mape)
cv_erro1
cv_erro1$reps
histogram(cv_erro1$reps)
hist(cv_erro1$reps,breaks="Fd")
cv_erro2

#stepwise variable selection
full.model <- lm(carbohydrate ~ ., data = carbohydrate)
ols_step_all_possible(full.model, details=TRUE)

# Lasso
#--------
y = carbohydrate$carbohydrate
x = as.matrix(carbohydrate[,c('age','weight','protein')])
fit = glmnet(x, y)
plot(fit, xvar='lambda')
cvfit = cv.glmnet(x, y)

final_model <- glmnet(x, y, alpha = 1, lambda = cvfit$lambda.min)
final_model
summary(final_model)
final_model_prs <- glmnet(x, y, alpha = 1, lambda = cvfit$lambda.1se)
summary(final_model_prs)
coef(final_model_prs)

final_coefs <- coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

# final model
final_model_prs <- glmnet(x, y, alpha = 1, lambda = cvfit$lambda.1se)
plot(y,type="l")
lines(predict(final_model_prs,newx = x),col="red")
cvfit$lambda.1se

# Using caret
ctrl <- trainControl(method = "cv",
                     number = 10,
                     selectionFunction = "tolerance",
                     tolerance=1) 
model <- train(x = x, 
               y = y,
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 2, length = 100)),
               trControl = ctrl)
model$bestTune


# ctrl <- trainControl(method = "cv",
#                      number = 10,
#                      selectionFunction = "tolerance")

ctrl <- trainControl(method = "cv",
                     number = 10,
                     selectionFunction = "oneSE")

model <- train(x = X, 
               y = y,
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 1, length = 100)),
               trControl = ctrl)

model$bestTune

#using a custome function
oneSE <- function(x, metric, maximize) {
  best <- which.max(x[, metric])
  bestValue <- x[best, metric]
  bestSE <- x[best, paste(metric, "SD", sep = "")]
  # Find simplest model within 1 SE of best
  candidates <- which(x[, metric] >= (bestValue - bestSE))
  # Return the one with highest penalty (simplest model)
  if(length(candidates) > 1) {
    # Assuming lambda is the tuning parameter
    return(candidates[which.max(x[candidates, "lambda"])])
  } else {
    return(candidates)
  }
}

ctrl <- trainControl(method = "cv", 
                     number = 10,
                     selectionFunction = oneSE) 
model <- train(x = x, 
               y = y,
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.001, 2, length = 100)),
               trControl = ctrl)
model$bestTune
