## Degress of freedom
#----------------------
library(psych)

# basic form for linear regression
lm_m1<-lm(mpg~disp+hp+wt,data=mtcars)
var<-sum(lm_m1$residuals^2)/(nrow(mtcars)-length(lm_m1$coefficients))
summary(lm_m1)
var
sqrt(var)
sqrt(var)/mean(mtcars$mpg)

#df
tr = function(A) { return(sum(diag(A))) }
(1/var)*tr(cov(mtcars$mpg,predict(lm_m1)))

# Naive approach
n<-100
B = 500
p<-20
x = matrix(rnorm(n*p),n,p)
beta = 2*runif(p)
y = x %*% beta + rnorm(n)


y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  x.b = x[index.b,]
  y.b = y[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x.b+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # way too big!! why??

# Better approach, residual boostrap
yhat = lm(y~x+0)$fitted
ehat = y-yhat

B = 500
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  y.b = yhat + ehat[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # much better

#test with spline
#-----------------
set.seed(123)
n <- 100
x <- seq(0, 10, length.out = n)
y <- 2 * sin(x) + 0.5 * x + rnorm(n, 0, 0.5)
data <- data.frame(x = x, y = y)
plot(x,y)

ml_spl<-smooth.spline(x, y)
ml_spl$df
plot(x,y)
lines(ml_spl, col = "red", lwd = 2)

#using formula (not sure)
residuals<-y-fitted(ml_spl)
rss <- sum(residuals^2)
sigma_h<-rss/(length(y)-ml_spl$df)
(1/sigma_h)*tr(cov(y,fitted(ml_spl)))

# using bootstrap
yhat<-fitted(ml_spl)
er<-y-yhat
n<-length(y)

B=1000
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  y.b = yhat + ehat[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = fitted(smooth.spline(x, y.b))
}
tr(cov(y.boot,yhat.boot)) # great!!!!

# find the variance of the model
set.seed(456)  
boot_samples <- bootstraps(data, times = 500)


