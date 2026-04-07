library(ISLR2)
library(MLmetrics)
library(boot)
library(splines)
dim(Auto)
ggplot(Auto, aes(weight, mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# linear regression
reg <- lm(mpg ~ weight, data = Auto)
plot(reg)

# quadratic regression
quadr <-  lm(mpg ~ poly(weight, 2,raw = TRUE), data = Auto)
summary(quadr)
quadr2 <-  lm(mpg ~ weight+I(weight^2), data = Auto)
summary(quadr2)
Yhat <-  predict(quadr)
ggplot(bind_cols(Auto, data.frame(Yhat)), aes(x = weight, y = mpg)) +
  geom_point() +
  geom_line(mapping = aes(x = weight, y = Yhat), 
            color = "blue", linewidth = 2)

# compare models
#delta[1] = raw cross-validation estimate of prediction error  
#delta[2] = bias-corrected cross-validation estimate
reg <- glm(mpg ~ weight, data = Auto)
quadr <- glm(mpg ~ poly(weight, 2), data = Auto)
set.seed(1234)
cv.glm(Auto, reg, K = 10)$delta^.5
cv.glm(Auto, quadr, K = 10)$delta^.5
predlm<-predict(reg)
MSE(predlm,Auto$mpg)
cubic<-glm(mpg ~ poly(weight, 3), data = Auto)
cv.glm(Auto, cubic, K = 10)$delta^.5

# broken line
Z <- Auto$weight < 3000 #pounds
broken_line <- lm(mpg ~ weight * Z, data = Auto)
summary(broken_line)
Yhat <-  predict(quadr)
Yhatb <- predict(broken_line)
ggplot(bind_cols(Auto, data.frame(Yhat, Yhatb)), aes(x = weight, y = mpg)) +
  geom_point(alpha = .3) +
  geom_line(mapping = aes(x = weight, y = Yhat), 
            color = "blue", linewidth = 1) +
  geom_line(mapping = aes(x = weight, y = Yhatb), 
            color = "orange", linewidth = 2)

# splines
spline1<-lm(mpg~bs(weight,knots=3000),Auto)
Yhat<-predict(spline1)
ggplot(bind_cols(Auto, data.frame(Yhat)), aes(x = weight, y = mpg)) +
  geom_point() +
  geom_line(mapping = aes(x = weight, y = Yhat), 
            color = "blue", linewidth = 2) 

spline3 <- lm(mpg ~ bs(weight, knots = c(2500, 3000, 4200)), data = Auto)
Yhat3 <- predict(spline3)
ggplot(bind_cols(Auto, data.frame(Yhat, Yhat3)), aes(x = weight, y = mpg)) +
  geom_point(alpha = .2) +
  geom_line(mapping = aes(x = weight, y = Yhat), 
            color = "blue", linewidth = 1) +
  geom_line(mapping = aes(x = weight, y = Yhat3), 
            color = "red", linewidth = 1) 

# select by CV
spline1<-spline1 <- glm(mpg ~ splines::bs(weight, knots =  3000), data = Auto)
cv.glm(Auto, spline1, K = 10)$delta^.5
spline2 <- glm(mpg ~ splines::bs(weight, knots = c(2500, 3000, 4200)), data = Auto)
cv.glm(Auto, spline2, K = 10)$delta^.5

# Natural splines
spline5 <- glm(mpg ~  splines::bs(weight, df = 5), data = Auto)
Yhatbs <- predict(spline5)

nspline <- glm(mpg ~  splines::ns(weight, df = 5), data = Auto)
Yhatns <- predict(nspline)

ggplot(bind_cols(Auto, data.frame(Yhatbs, Yhatns)), aes(x = weight, y = mpg)) +
  geom_point(alpha = .4) +
  geom_line(mapping = aes(x = weight, y = Yhatbs), 
            color = "blue", linewidth = 1) +
  geom_line(mapping = aes(x = weight, y = Yhatns), 
            color = "red", linewidth = 1, lty = 1) #natural spline


# smoothing spline
## linear regression
ss2 <- smooth.spline(Auto$weight, Auto$mpg, df = 2)
Yhat2 <- predict(ss2)
ggplot(Auto, aes(x = weight, y = mpg)) +
  geom_point(alpha = .2) +
  geom_line(data = as.data.frame(Yhat2), mapping = aes(x = x, y = y), 
            color = "blue", linewidth = 2) 

ss3 <- smooth.spline(Auto$weight, Auto$mpg, df = 3)
ss5 <- smooth.spline(Auto$weight, Auto$mpg, df = 5)
Yhat3 <- predict(ss3)
Yhat5 <- predict(ss5)
ggplot(Auto, aes(x = weight, y = mpg)) +
  geom_point(alpha = .2) +
  geom_line(data = as.data.frame(Yhat2), mapping = aes(x = x, y = y), 
            color = "blue", linewidth = 1) +
  geom_line(data = as.data.frame(Yhat3), mapping = aes(x = x, y = y), 
            color = "red", linewidth = 1) +
  geom_line(data = as.data.frame(Yhat5), mapping = aes(x = x, y = y), 
            color = "green", linewidth = 1) 


# let the function pick
ss <- smooth.spline(Auto$weight, Auto$mpg) #GCV
ss$df
ss <- smooth.spline(Auto$weight, Auto$mpg, cv = TRUE) #LOOCV
ss$df
Auto_distinct <- distinct(Auto, weight, .keep_all = TRUE) #LOOCV with Unique X
ss <- smooth.spline(Auto_distinct$weight, Auto_distinct$mpg, cv = TRUE)
ss$df













