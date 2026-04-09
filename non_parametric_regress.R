library(np)
library(fANCOVA)
# Generate some data to test the implementation
set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 * cos(x)
X <- rnorm(n, sd = 2)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)
h <- 0.5

# 1.- Nadaraya-Watson
#---------------------
#bw <- npregbw(xdat = X, ydat = Y)  
#mNW <- npreg(bws = bw)
mNW <- npreg(Y~X)
pNW <- predict(mNW,newdata=data.frame(X=xGrid))

# Plot data
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(xGrid,pNW , col = 2)
legend("top", legend = c("True regression", "Nadaraya-Watson"),
       lwd = 2, col = 1:2)

# 2.- Local polynomial estimator
#----------------------------------
set.seed(123456)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^3 * sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

h <- 0.25
lp0 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = 0,
                           range.x = c(-10, 10), gridsize = 500)
lp1 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = h, degree = 1,
                           range.x = c(-10, 10), gridsize = 500)
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1)
  lines(lp0$x, lp0$y, col = 2)
  lines(lp1$x, lp1$y, col = 3)
  legend("bottom", legend = c("True regression", "Local constant (locpoly)",
                           "Local linear (locpoly)"),
         lwd = 2, col = c(1:3, 2:3), lty = c(rep(1, 3), rep(2, 2)))
}

# loess fits
span <- 0.25 # The default span is 0.75, which works very bad in this scenario
lo0 <- loess(Y ~ X, degree = 0, span = span)
lo1 <- loess(Y ~ X, degree = 1, span = span)
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1)
  lines(xGrid, predict(lo0, newdata = data.frame(X = xGrid)), col = 2, lty = 2)
  lines(xGrid, predict(lo1, newdata = data.frame(X = xGrid)), col = 3, lty = 2)
  legend("bottom", legend = c("True regression","Local constant (loess)",
                              "Local linear (loess)"),
         lwd = 2, col = c(1:3, 2:3), lty = c(rep(1, 3), rep(2, 2)))
}

# both
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1)
  lines(lp0$x, lp0$y, col = 2)
  lines(lp1$x, lp1$y, col = 3)
  lines(xGrid, predict(lo0, newdata = data.frame(X = xGrid)), col = 2, lty = 2)
  lines(xGrid, predict(lo1, newdata = data.frame(X = xGrid)), col = 3, lty = 2)
  legend("bottom", legend = c("True regression", "Local constant (locpoly)",
                              "Local linear (locpoly)", "Local constant (loess)",
                              "Local linear (loess)"),
         lwd = 2, col = c(1:3, 2:3), lty = c(rep(1, 3), rep(2, 2)))
}

# Bandwidth selection
# DPI selector
hDPI <- KernSmooth::dpill(x = X, y = Y)

# Fits
lp1 <- KernSmooth::locpoly(x = X, y = Y, bandwidth = 0.25, degree = 0,
                           range.x = c(-10, 10), gridsize = 500)
lp1DPI <- KernSmooth::locpoly(x = X, y = Y, bandwidth = hDPI, degree = 1,
                              range.x = c(-10, 10), gridsize = 500)

# Compare fits
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1)
  lines(lp1$x, lp1$y, col = 2)
  lines(lp1DPI$x, lp1DPI$y, col = 3)
  legend("bottom", legend = c("True regression", "Local linear",
                              "Local linear (DPI)"),
         lwd = 2, col = 1:3)
}
# Using the evaluation points
set.seed(12345)
n <- 100
eps <- rnorm(n, sd = 2)
m <- function(x) x^2 + sin(x)
X <- rnorm(n, sd = 1.5)
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# np::npregbw computes by default the least squares CV bandwidth associated with
# a local constant fit
bw0 <- np::npregbw(formula = Y ~ X)

# Multiple initial points can be employed for minimizing the CV function (for
# one predictor, defaults to 1)
bw0 <- np::npregbw(formula = Y ~ X, nmulti = 2)

# The "rbandwidth" object contains many useful information, see ?np::npregbw for
# all the returned objects
bw0

kre0 <- np::npreg(bw0)
kre0

plot(kre0$eval$X, kre0$mean)
kre0 <- np::npreg(bw0, exdat = xGrid)

plot(kre0, col = 2, type = "o")
points(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(kre0$eval$xGrid, kre0$mean, col = 3, type = "o", pch = 16, cex = 0.5)

# Local linear fit -- find first the CV bandwidth
bw1 <- np::npregbw(formula = Y ~ X, regtype = "ll")
# regtype = "ll" stands for "local linear", "lc" for "local constant"

# Local linear fit
kre1 <- np::npreg(bw1, exdat = xGrid)

# Comparison
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(kre0$eval$xGrid, kre0$mean, col = 2)
lines(kre1$eval$xGrid, kre1$mean, col = 3)
legend("top", legend = c("True regression", "Nadaraya-Watson", "Local linear"),
       lwd = 2, col = 1:3)



## add loess
lo <- loess(Y ~ X, span = 0.3)   # adjust span for smoothness
lo_pred <- predict(lo, newdata = data.frame(X = xGrid))
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1, lwd = 2)
  lines(kre0$eval$xGrid, kre0$mean, col = 2, lwd = 2)
  lines(kre1$eval$xGrid, kre1$mean, col = 3, lwd = 2)
  lines(xGrid, lo_pred, col = 4, lwd = 2)
  
  legend("top",
         legend = c("True regression", "Nadaraya-Watson", "Local linear", "LOESS"),
         lwd = 2,
         col = 1:4)  
}

fit <- loess.as(X, Y, criterion = "gcv")  # or "aicc"
fit$pars$span   # selected span
lo_pred <- predict(fit, newdata = xGrid)
{
  plot(X, Y)
  rug(X, side = 1); rug(Y, side = 2)
  lines(xGrid, m(xGrid), col = 1, lwd = 2)
  lines(kre0$eval$xGrid, kre0$mean, col = 2, lwd = 2)
  lines(kre1$eval$xGrid, kre1$mean, col = 3, lwd = 2)
  lines(xGrid, lo_pred, col = 4, lwd = 2)
  
  legend("top",
         legend = c("True regression", "Nadaraya-Watson", "Local linear", "LOESS"),
         lwd = 2,
         col = 1:4)  
}

## More sophisticated bandwith selection
#----------------------------------------
# Generate some data with bimodal density
set.seed(12345)
n <- 100
eps <- rnorm(2 * n, sd = 2)
m <- function(x) x^2 * sin(x)
X <- c(rnorm(n, mean = -2, sd = 0.5), rnorm(n, mean = 2, sd = 0.5))
Y <- m(X) + eps
xGrid <- seq(-10, 10, l = 500)

# Constant bandwidth
bwc <- np::npregbw(formula = Y ~ X, bwtype = "fixed", regtype = "ll")
krec <- np::npreg(bwc, exdat = xGrid)

# Variable bandwidths
bwg <- np::npregbw(formula = Y ~ X, bwtype = "generalized_nn", regtype = "ll")
kreg <- np::npreg(bwg, exdat = xGrid)
bwa <- np::npregbw(formula = Y ~ X, bwtype = "adaptive_nn", regtype = "ll")
krea <- np::npreg(bwa, exdat = xGrid)

# Comparison
plot(X, Y)
rug(X, side = 1); rug(Y, side = 2)
lines(xGrid, m(xGrid), col = 1)
lines(krec$eval$xGrid, krec$mean, col = 2)
lines(kreg$eval$xGrid, kreg$mean, col = 3)
lines(krea$eval$xGrid, krea$mean, col = 4)
legend("top", legend = c("True regression", "Fixed", "Generalized NN",
                         "Adaptive NN"),
       lwd = 2, col = 1:4)


























