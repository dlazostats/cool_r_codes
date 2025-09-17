# Create a simple nonlinear example
set.seed(0)
n = 500
x = sort(runif(n,0,6*pi))
r = x*sin(x) 
y = r + rnorm(n)

plot(x,y,col="gray50")
lines(x,r,col="blue",lwd=2)

# Regression splines
library(splines)
knots = seq(2,16,length=6)
G = bs(x,knots=knots,degree=3)
regmod = lm(y~G)

plot(x,y)
lines(x,regmod$fitted,lwd=3,col="blue")
abline(v=knots,lty=3,lwd=3)

# Smoothing splines
dfs = c(7,25,250)
splinemod1 = smooth.spline(x,y,df=dfs[1])
splinemod2 = smooth.spline(x,y,df=dfs[2])
splinemod3 = smooth.spline(x,y,df=dfs[3])

par(mfrow=c(2,3))
plot(x,y,col="gray50",main=paste("df =",dfs[1]))
lines(splinemod1$x,splinemod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("df =",dfs[2]))
lines(splinemod2$x,splinemod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("df =",dfs[3]))
lines(splinemod3$x,splinemod3$y,col="blue",lwd=2)

# Kernel regression
bws = c(5,1,0.1)
kernmod1 = ksmooth(x,y,kernel="normal",bandwidth=bws[1])
kernmod2 = ksmooth(x,y,kernel="normal",bandwidth=bws[2])
kernmod3 = ksmooth(x,y,kernel="normal",bandwidth=bws[3])

#par(mfrow=c(1,3))
plot(x,y,col="gray50",main=paste("bandwidth =",bws[1]))
lines(kernmod1$x,kernmod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[2]))
lines(kernmod2$x,kernmod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[3]))
lines(kernmod3$x,kernmod3$y,col="blue",lwd=2)

# Claude example
#----------------
library(splines)
library(ggplot2)
library(mgcv)

set.seed(123)
x <- seq(0, 10, length.out = 100)
y <- sin(x) + rnorm(100, 0, 0.2)

# Fit different types of splines
# Natural spline
ns_fit <- lm(y ~ ns(x, df = 5))

# B-spline
bs_fit <- lm(y ~ bs(x, df = 5))

# Smoothing spline
smooth_fit <- smooth.spline(x, y)

# Plot results
par(mfrow=c(1,1))
plot(x, y, pch = 16, col = "gray")
lines(x, predict(ns_fit), col = "red", lwd = 2)
lines(x, predict(bs_fit), col = "blue", lwd = 2)
lines(smooth_fit, col = "green", lwd = 2)
legend("topright", c("Natural", "B-spline", "Smoothing"), 
       col = c("red", "blue", "green"), lwd = 2)

gam_fit <- gam(y ~ s(x))
summary(gam_fit)
# You can also set basis dimension and let it choose within that
gam_fit2 <- gam(y ~ s(x, k = 20)) 
summary(gam_fit2)
plot(x, y, pch = 16, col = "gray")
lines(x, predict(gam_fit2), col = "red", lwd = 2)

# Automatic knot selection via cross-validation
par(mfrow=c(1,1))
smooth_fit <- smooth.spline(x, y, cv = TRUE)
smooth_fit$df
plot(x, y, pch = 16, col = "gray")
lines(smooth_fit, col = "green", lwd = 2)

x<-mtcars$mpg
y<-mtcars$disp
smooth_fit <- smooth.spline(x, y, cv = TRUE)
smooth_fit$df
plot(x, y, pch = 16, col = "gray")
lines(smooth_fit, col = "green", lwd = 2)

gam_fit <- gam(y ~ s(x))
summary(gam_fit)
gam.check(gam_fit) 
plot(gam_fit)
# You can also set basis dimension and let it choose within that
gam_fit2 <- gam(y ~ s(x, k = 20)) 
plot(x, y, pch = 16, col = "gray")
lines(x, fitted(gam_fit), col = "red", lwd = 2)


library(gratia)

# Simple smooth plot
draw(gam_fit)

# More customization
draw(gam_fit) +
  labs(title = "GAM Smooth with gratia",
       subtitle = paste("EDF:", round(sum(gam_fit$edf), 2))) +
  theme_bw()

# For multiple smooths
# Example with multiple predictors
set.seed(123)
n <- 200
x1 <- runif(n, 0, 10)
x2 <- runif(n, 0, 5)
y <- sin(x1) + x2^2 * 0.1 + rnorm(n, 0, 0.3)

# Fit GAM with multiple smooths
gam_multi <- gam(y ~ s(x1) + s(x2))

# Plot all smooth terms
par(mfrow = c(1, 2))
plot(gam_multi, pages = 1)  # All plots on one page

# Or plot individually with more control
plot(gam_multi, select = 1, shade = TRUE, main = "Effect of x1")
plot(gam_multi, select = 2, shade = TRUE, main = "Effect of x2")

draw(gam_multi)

gam_2d <- gam(y ~ s(x1, x2))

# 3D surface plot
vis.gam(gam_2d, view = c("x1", "x2"), plot.type = "persp", 
        theta = 30, phi = 20, main = "2D Smooth Surface")

# Contour plot
vis.gam(gam_2d, view = c("x1", "x2"), plot.type = "contour", 
        main = "Contour Plot", color = "topo")


# Compare different smoothness levels
set.seed(123)
x <- seq(0, 10, length.out = 200)
y <- sin(x) + 0.3 * x + rnorm(200, 0, 0.3)

gam1 <- gam(y ~ s(x, k = 5))   # Fewer basis functions
gam2 <- gam(y ~ s(x, k = 20))  # More basis functions

# Plot comparison
par(mfrow = c(1, 2))
plot(gam1, main = "k = 5", shade = TRUE)
plot(gam2, main = "k = 20", shade = TRUE)

# Model comparison statistics
AIC(gam1, gam2)
anova(gam1, gam2, test = "F")
