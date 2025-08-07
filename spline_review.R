U <- seq(0, 1, length=100)
D <- 3 # degree of series
K <- 5 # number of knots
knots <- (1:K) / (K+1) # creates a series of K equidistant knots
X1 <- outer (U, 1:D, "^") #a s in eq.2
X2 <- outer (U, knots,">") * outer (U, knots, "-")^D # as in eq.3
Bt <- cbind (X1, X2)
matplot(U, Bt, 'l', lwd=2, col= 1:ncol(Bt))

x <- seq(0, 1, length=100)
library(splines)
par(mfrow= c(2, 2))
matplot(bs(x, degree=1, knots=c(0,.5,1)), type='l', main= "Degree=1")
matplot(bs(x, degree=2, knots=c(0,.5,1)), type='l', main= "Degree=2")
matplot(bs(x, degree=1, knots=c(0,.25,.5,1)), type='l', main= "Degree=1")
matplot(bs(x, degree=3, knots=c(0,.25,.5,1)), type='l', main= "Degree=3")

par(mfrow= c(2, 2))
matplot(ns(x,df=2), ylab= "", type='l', main="Df=2")
matplot(ns(x,df=3), ylab= "", type='l', main="Df=3")
matplot(ns(x,df=4), ylab= "", type='l', main="Df=4")
matplot(ns(x,df=5), ylab= "", type='l', main="Df=5")

## function to define spline basis
pbase <- function(x, p) {
  u <- (x - min(x)) / (max(x) - min(x))
  u <- 2 * (u - 0.5)
  P <- outer(u, seq(0, p, by = 1), "^")
  P
}
## Data available in http://mfp.imbi.uni-freiburg.de/book
## load data into R session and assign x <- age ; y <- lntriceps
dta_lk<-read.delim("clipboard")
x<-dta_lk$age
y<-dta_lk$lntriceps
## fit models
fit.poly <- lm(y ~ poly(x)) #polynomial spline
fit.bs <- lm(y ~ bs(x) ) # bspline
fit.ns <- lm(y ~ ns(x) ) #natural spline
fit.sp <- smooth.spline(y ~ x)
## add fit lines to the plot
lines(x, predict(fit.poly, data.frame(x=x)), col=1, lwd=2)
lines(x, predict(fit.bs, data.frame(x=x)), col=2, lwd=2)
lines(x, predict(fit.ns, data.frame(x=x)), col=3, lwd=2)
lines(fit.sp, col=4, lwd=2)
legend(0.05, 3.8, "default values", col=1, bty="n")
par(mar= c(5, 4, vdist, hdist))
plot(x, y, ylab="Triceps skinfold thickness in mm (log)", xlab="Age in years", axes=FALSE)

library(VGAM)
library(gam)
library(gamlss)
set.seed(0)
n <- 400
x <- 0:(n-1) / (n-1)
f <- -3.5+0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
y <- f + rnorm(n, 0, sd = 2)
fit.gam <- gam(y~ bs(x))
fit.vgam<- vgam(y~ bs(x), uninormal)
fit.gamlss<- gamlss(y~ bs(x), control=gamlss.control(trace=FALSE))
summary(fit.gam)

# Get fitted values
fitted.gam <- fitted(fit.gam)
fitted.vgam <- fitted(fit.vgam)
fitted.gamlss <- fitted(fit.gamlss)

# Plot
plot(x, y, col = "grey80", pch = 16, main = "Model Fits vs True Function",
     xlab = "x", ylab = "y")
lines(x, f, col = "black", lwd = 2, lty = 2)                  # True function
lines(x, fitted.gam, col = "blue", lwd = 2)                   # GAM
lines(x, fitted.vgam, col = "green", lwd = 2, lty = 3)        # VGAM
lines(x, fitted.gamlss, col = "red", lwd = 2, lty = 4)        # GAMLSS
legend("topright", legend = c("True f", "GAM", "VGAM", "GAMLSS"),
       col = c("black", "blue", "green", "red"),
       lty = c(2, 1, 3, 4), lwd = 2)

gam.bs <- gam(y~bs(x))
gam.ns <- gam(y~ns(x,df=3))
gam.bsK <- gam(y~bs(x, knots=seq(0, 1, by=.2)))
gam.nsK <- gam(y~ns(x, knots=seq(0.2, .8, by=.2)))
## define plotting parameters
vdist <- hdist <- 0.2
layout(matrix(1:4, 2, 2, byrow=TRUE), widths= c(10, 10), heights= c(10, 10))
par(mar= c(vdist, 4, 3, hdist))
plot(x, y, ylab="y", xlab="", axes=FALSE)
axis(2); axis(3); box()
lines(x, f, lwd=2, lty=2)
lines(x, predict(gam.bs), col=2, lwd=2)
lines(x, predict(gam.ns), col=3, lwd=2)
legend(0.15,-3,c("Real", "bs", "ns"),
       col=c(1, 2, 3), lty=1, bty="n")
par(mar= c(vdist, hdist, 3, 4))
plot(x,y,ylab="",xlab="",axes=FALSE)
axis(3); axis(4); box()
lines(x, f, lwd=2, lty=2)
lines(x, predict(gam.bsK), col=2, lwd=2)
lines(x, predict(gam.nsK), col=3, lwd=2)
legend(0.15,-3,c("Real", "bs", "ns"),
       col=c(1, 2, 3), lty=1, bty="n")
legend("topright", "Four interior knots", col=1, lty=0, bty="n")
par(mar= c(5, 4, vdist, hdist))
plot(x, y, ylab="", xlab="x", axes=FALSE)
lines(x, f, lwd=2, lty=2)
axis(1); axis(2); box()
fit.gam <- gam(y~s(x))
t3 <- predict(fit.gam)
lines(x, t3, col=2, lwd=2)



library(VGAM)
library(gam)
library(gamlss)
library(splines)

# Set up data
set.seed(0)
n <- 400
x <- 0:(n-1) / (n-1)
f <- -3.5 + 0.2*x^11 * (10*(1-x))^6 + 10*(10*x)^3 * (1-x)^10
y <- f + rnorm(n, 0, sd = 2)

# Fit models
fit.gam <- gam(y ~ bs(x))
fit.vgam <- vgam(y ~ bs(x), uninormal)
fit.gamlss <- gamlss(y ~ bs(x), control = gamlss.control(trace = FALSE))

# Get fitted values
fitted.gam <- fitted(fit.gam)
fitted.vgam <- fitted(fit.vgam)
fitted.gamlss <- fitted(fit.gamlss)

# Plot
plot(x, y, col = "grey80", pch = 16, main = "Model Fits vs True Function",
     xlab = "x", ylab = "y")
lines(x, f, col = "black", lwd = 2, lty = 2)                  # True function
lines(x, fitted.gam, col = "blue", lwd = 2)                   # GAM
lines(x, fitted.vgam, col = "green", lwd = 2, lty = 3)        # VGAM
lines(x, fitted.gamlss, col = "red", lwd = 2, lty = 4)        # GAMLSS
legend("topright", legend = c("True f", "GAM", "VGAM", "GAMLSS"),
       col = c("black", "blue", "green", "red"),
       lty = c(2, 1, 3, 4), lwd = 2)



