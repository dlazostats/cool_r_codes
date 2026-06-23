set.seed(123)

n <- 500
x <- sort(runif(n, 0, 10))

# True mean function
f_true <- sin(x) + x/5

# Variance increases with x (heteroscedasticity)
sigma_x <- 0.2 + 0.5 * (x / max(x))

y <- f_true + rnorm(n, 0, sigma_x)

df <- data.frame(x = x, y = y)

library(dbarts)

bart_fit <- bart(
  x.train = as.matrix(df$x),
  y.train = df$y,
  ndpost = 1000,
  nskip = 200
)

# Posterior mean predictions
yhat_mean <- colMeans(bart_fit$yhat.train)

# Posterior std (uncertainty)
yhat_sd <- apply(bart_fit$yhat.train, 2, sd)

# Residuals
residuals <- df$y - yhat_mean

# Model log-variance using another BART
log_res2 <- log(residuals^2 + 1e-6)

bart_var <- bart(
  x.train = as.matrix(df$x),
  y.train = log_res2,
  ndpost = 1000,
  nskip = 200
)

# Predicted variance function
log_var_hat <- colMeans(bart_var$yhat.train)
sigma_hat <- sqrt(exp(log_var_hat))


plot(df$x, df$y, col = "gray", pch = 16, main = "BART vs Heteroscedastic BART")

# True mean
lines(x, f_true, col = "black", lwd = 2)

# Standard BART mean
lines(x, yhat_mean, col = "blue", lwd = 2)

# Constant uncertainty (approx)
lines(x, yhat_mean + 2*yhat_sd, col = "blue", lty = 2)
lines(x, yhat_mean - 2*yhat_sd, col = "blue", lty = 2)

# HBART-like uncertainty
lines(x, yhat_mean + 2*sigma_hat, col = "red", lwd = 2)
lines(x, yhat_mean - 2*sigma_hat, col = "red", lwd = 2)

legend("topleft",
       legend = c("True", "BART", "BART CI", "HBART CI"),
       col = c("black", "blue", "blue", "red"),
       lty = c(1,1,2,1))

plot(yhat_mean, residuals,
     main = "Residuals vs Predictions",
     pch = 16, col = "darkgray")

abline(h = 0, col = "red", lwd = 2)


#using brms
library(brms)

fit <- brm(
  bf(y ~ s(x), sigma ~ s(x)),
  data = df,
  family = gaussian()
)



