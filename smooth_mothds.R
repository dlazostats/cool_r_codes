# Smoothing methods
#--------------------
library(np)
library(splines)
library(mgcv)
library(yardstick)
library(kernlab)
library(MLmetrics)

fun_plob<-function(nome){
  plot(x, y,
       pch = 16,
       col = "gray",
       main = nome,
       xlab = "x",
       ylab = "y")
  lines(x_grid, f_true(x_grid),
        lwd = 3,
        col = "black",
        lty = 2)
}

# generar datos
set.seed(123)
n <- 150
x <- sort(runif(n, 0, 10))
f_true <- function(x) {
  sin(x) + 0.5*cos(2*x)
}
y <- f_true(x) + rnorm(n, sd = 0.4)
data <- data.frame(x, y)
x_grid <- seq(0, 10, length = 300)
plot(x,y)

# a) Kernel Regression (Nadaraya-Watson)
kernel_model <- npreg(y ~ x, data = data)
kernel_pred <- predict(kernel_model,
                       newdata = data.frame(x = x_grid))
kernel_model
RMSE(kernel_pred,f_true(x_grid))
fun_plob("Kernel")
lines(x_grid, kernel_pred, col = "orange", lwd = 2)

# b) Local polynomial regression (Loess)
loess_model <- loess(y ~ x, data = data, span = 0.3)
loess_pred <- predict(loess_model,
                      newdata = data.frame(x = x_grid))
loess_model
rmse_vec(loess_pred %>% as.numeric(),f_true(x_grid))
fun_plob("loess")
lines(x_grid, loess_pred, col = "orange", lwd = 2)

# Regression splines
spline_model <- lm(y ~ bs(x, df = 8), data = data)
spline_pred <- predict(spline_model,
                       newdata = data.frame(x = x_grid))
spline_model
RMSE(spline_pred,f_true(x_grid))
fun_plob("splines")
lines(x_grid, spline_pred, col = "red", lwd = 2)

# GAM
gam_model <- gam(y ~ s(x), data = data)
gam_pred <- predict(gam_model,
                    newdata = data.frame(x = x_grid))
gam_model
RMSE(gam_pred,f_true(x_grid))
fun_plob("gam")
lines(x_grid, gam_pred, col = "red", lwd = 2)

# Gaussian Process Regression
gp_model <- gausspr(as.matrix(x), y, kernel = "rbfdot")
gp_pred <- predict(gp_model,
                   as.matrix(x_grid))
gp_model
RMSE(gp_pred,f_true(x_grid))
fun_plob("Gaussian Process")
lines(x_grid, gp_pred, col = "red", lwd = 2)

# campare metrics
res_dt<-data.frame(
  kernel=RMSE(kernel_pred,f_true(x_grid)),
  loess=rmse_vec(loess_pred %>% as.numeric(),f_true(x_grid)),
  spline=RMSE(spline_pred,f_true(x_grid)),
  gam=RMSE(gam_pred,f_true(x_grid)),
  gp=RMSE(gp_pred,f_true(x_grid)))
res_dt

# Plot all results
plot(x, y,
     pch = 16,
     col = "gray",
     main = "Comparison of Nonparametric Smoothing Methods",
     xlab = "x",
     ylab = "y")

lines(x_grid, f_true(x_grid),
      lwd = 3,
      col = "black",
      lty = 2)

lines(x_grid, kernel_pred, col = "orange", lwd = 2)
lines(x_grid, loess_pred, col = "blue", lwd = 2)
lines(x_grid, spline_pred, col = "red", lwd = 2)
lines(x_grid, smoothing_pred, col = "darkgreen", lwd = 2)
lines(x_grid, gam_pred, col = "purple", lwd = 2)
lines(x_grid, gp_pred, col = "brown", lwd = 2)

legend("topright",
       legend = c("True Function",
                  "Kernel Regression",
                  "LOESS",
                  "Regression Spline",
                  "Smoothing Spline",
                  "GAM",
                  "Gaussian Process"),
       col = c("black","orange","blue","red",
               "darkgreen","purple","brown"),
       lty = c(2,1,1,1,1,1,1),
       lwd = 2)


