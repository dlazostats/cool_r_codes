# ---- Gaussian Process regression from scratch ----
set.seed(42)

# Squared-exponential kernel
rbf_kernel <- function(X1, X2, l = 1, sigma_f = 1) {
  # X1: n1-vector, X2: n2-vector -> n1 x n2 covariance matrix
  sqdist <- outer(X1, X2, function(a, b) (a - b)^2)
  sigma_f^2 * exp(-0.5 * sqdist / l^2)
}

# ---- Generate some noisy training data ----
f_true <- function(x) sin(x) + 0.3 * x          # the unknown function
n <- 12
X_train <- sort(runif(n, -5, 5))
sigma_n <- 0.2                                   # observation noise sd
y_train <- f_true(X_train) + rnorm(n, 0, sigma_n)
plot(X_train, y_train)

# Test points where we want predictions
X_test <- seq(-6, 6, length.out = 200)

# ---- GP posterior ----
gp_predict <- function(X_test, X_train, y_train,
                       l = 1, sigma_f = 1, sigma_n = 0.2) {
  K    <- rbf_kernel(X_train, X_train, l, sigma_f) +
    sigma_n^2 * diag(length(X_train))      # add noise on the diagonal
  K_s  <- rbf_kernel(X_train, X_test,  l, sigma_f)
  K_ss <- rbf_kernel(X_test,  X_test,  l, sigma_f)
  
  K_inv <- solve(K)                              # for clarity; use Cholesky in practice
  mu   <- t(K_s) %*% K_inv %*% y_train           # posterior mean
  cov  <- K_ss - t(K_s) %*% K_inv %*% K_s        # posterior covariance
  list(mu = as.vector(mu), var = pmax(diag(cov), 0))
}

pred <- gp_predict(X_test, X_train, y_train,
                   l = 1, sigma_f = 1, sigma_n = sigma_n)

# ---- Plot mean +/- 2 sd (~95% credible band) ----
sd_pred <- sqrt(pred$var)
plot(X_test, pred$mu, type = "l", lwd = 2, col = "blue",
     ylim = range(c(pred$mu + 2*sd_pred, pred$mu - 2*sd_pred, y_train)),
     xlab = "x", ylab = "f(x)", main = "GP regression")
polygon(c(X_test, rev(X_test)),
        c(pred$mu + 2*sd_pred, rev(pred$mu - 2*sd_pred)),
        col = rgb(0, 0, 1, 0.15), border = NA)          # uncertainty band
curve(f_true, add = TRUE, lty = 2, col = "darkgreen")   # true function
points(X_train, y_train, pch = 19)                      # observations
legend("topleft", bty = "n",
       legend = c("GP mean", "95% band", "true f", "data"),
       col = c("blue", rgb(0,0,1,0.3), "darkgreen", "black"),
       lty = c(1, NA, 2, NA), pch = c(NA, 15, NA, 19))

# using a package
library(kernlab)

d <- data.frame(x = X_train, y = y_train)
fit <- gausspr(y ~ x, data = d, kernel = "rbfdot",
               kpar = "automatic", var = sigma_n^2)
mu <- predict(fit, data.frame(x = X_test))
plot(X_test, mu, type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "f(x)")
points(X_train, y_train, pch = 19)


