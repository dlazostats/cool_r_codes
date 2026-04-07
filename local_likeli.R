# local likelihood
#-----------------
library("locfit")
set.seed(123)

n <- 200
x <- runif(n, 0, 10)
y <- sin(x) + rnorm(n, sd = 0.3)

data <- data.frame(x, y)
plot(x,y)

fit <- locfit(y ~ lp(x), data = data) #local polynomial smoother
plot(x, y, pch = 16, col = "gray")
plot(fit, col = "blue", lwd = 2, add = TRUE)

# control smoothing parameter
fit_smooth <- locfit(y ~ lp(x, nn = 0.7), data = data)
fit_wiggly <- locfit(y ~ lp(x, nn = 0.2), data = data)
x_grid <- seq(min(x), max(x), length=200)

plot(x, y, pch = 16, col = "gray")
pred_smooth <- predict(fit_smooth, newdata = data.frame(x=x_grid))
pred_wiggly <- predict(fit_wiggly, newdata = data.frame(x=x_grid))

plot(x, y, pch=16, col="gray")
lines(x_grid, pred_smooth, col="blue", lwd=2)
lines(x_grid, pred_wiggly, col="red", lwd=2)
legend("topright",
       legend=c("nn=0.7","nn=0.2"),
       col=c("blue","red"),
       lwd=2)

# predict
x_new <- seq(0, 10, length = 100)
pred <- predict(fit, newdata = data.frame(x = x_new))
plot(x, y, pch = 16, col = "gray")
lines(x_new, pred, col = "blue", lwd = 2)

fit_quad <- locfit(y ~ lp(x, deg = 2), data = data) # deg=0 local constant (Nadaraya–Watson)

# confidence interval
fit <- locfit(y ~ lp(x), data = data)
pred <- predict(fit,
                newdata = data.frame(x = x_new),
                se.fit = TRUE)
upper <- pred$fit + 2 * pred$se.fit
lower <- pred$fit - 2 * pred$se.fit

plot(x, y, pch = 16, col = "gray")
lines(x_new, pred$fit, col = "blue", lwd = 2)
lines(x_new, upper, col = "red", lty = 2)
lines(x_new, lower, col = "red", lty = 2)

# degrees of freedom
set.seed(1)
n <- 100
x <- sort(runif(n))
y <- sin(2*x) + rnorm(n,0,0.2)
library(splines)
fit <- smooth.spline(x,y)
fit$df
fit1 <- locfit(y ~ lp(x, nn=0.2))
fit2 <- locfit(y ~ lp(x, nn=0.6))
fit1
fit2
