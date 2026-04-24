## PIIT
#--------
library(quantregForest)
library(goftest)

# 1.- Basic PIT
#---------------
x<-rnorm(1000,mean=2,sd=4)
y<-pnorm(x,mean=2,sd=4)

# graphical test
a <- min(y)
b <- max(y)
hist(y,breaks="FD",freq=F)
curve(dunif(x, min = a, max = b), add = TRUE, lwd = 2,col="red")

# statistical test
ks.test(y, "punif", min = a, max = b)
ad.test(y, null = "punif", min = a, max = b)

# 2.- Simulation from a known conditional disitribution
#--------------------------------------------------------
set.seed(123)
n  <- 5000
X  <- runif(n, -2, 2)
Y  <- rnorm(n, mean = X, sd = 1)
df <- data.frame(X = X, Y = Y)

# fit a correc onditional model
fit <- lm(Y ~ X, data = df)
mu_hat    <- predict(fit)                     # estimated conditional mean
sigma_hat <- summary(fit)$sigma               # estimated conditional sd

# fit conditional pit
U <- pnorm((df$Y - mu_hat) / sigma_hat)

hist(U, breaks = 30, freq = FALSE,
     main = "Conditional PIT (correct model)",
     xlab = "U")
curve(dunif(x), add = TRUE, lwd = 2, col = "red")

# statistical test
ks.test(U, "punif", min = a, max = b)

# What if the model is misspecified
fit_wrong <- lm(Y ~ 1, data = df)   
mu_hat_wrong    <- predict(fit_wrong)
sigma_hat_wrong <- summary(fit_wrong)$sigma
U_wrong <- pnorm((df$Y - mu_hat_wrong) / sigma_hat_wrong)

hist(U_wrong, breaks = 30, freq = FALSE,
     main = "Conditional PIT (correct model)",
     xlab = "U")
curve(dunif(x), add = TRUE, lwd = 2, col = "red")
ks.test(U_wrong, "punif", min = a, max = b)

#3.- Using Random Forest
#-------------------------
#Simulate data with a non-Gaussian conditional distribution
set.seed(123)
n <- 3000
X <- runif(n, -2, 2)
sigma <- 0.5 + 0.5 * abs(X)
Y <- rnorm(n, mean = X, sd = sigma)
df <- data.frame(X = X, Y = Y)

# fit quantile regression
qrf_fit <- quantregForest(
  x = as.matrix(df$X),
  y = df$Y,
  ntree = 500
)
qrf_fit

#We approximate the conditional CDF by evaluating many quantiles
tau_grid <- seq(0.01, 0.99, by = 0.005)
Q_hat <- predict(qrf_fit,
                 newdata = as.matrix(df$X),
                 what = tau_grid)

# Function to compute PIT for one observation
pit_one <- function(y, q_vec, tau_vec) {
  if (y <= min(q_vec)) return(min(tau_vec))
  if (y >= max(q_vec)) return(max(tau_vec))
  approx(x = q_vec, y = tau_vec, xout = y, ties = "ordered")$y
}
U_qrf <- numeric(n)
for (i in 1:n) {
  U_qrf[i] <- pit_one(df$Y[i], Q_hat[i, ], tau_grid)
}

hist(U_qrf, breaks = 30, freq = FALSE,
     main = "Conditional PIT - Quantile Regression Forest",
     xlab = "U")
curve(dunif(x), add = TRUE, lwd = 2, col = "red")

#Check independence from X A crucial conditional check:
plot(df$X, U_qrf, pch = 20, col = rgb(0,0,0,0.3),
     xlab = "X", ylab = "PIT U",
     main = "PIT should be independent of X")

#A useful formal diagnostic: conditional coverage
# this should be close to tau_test
tau_test <- 0.1
mean(U_qrf <= tau_test)
sapply(c(0.1, 0.25, 0.5, 0.75, 0.9), function(t)
  mean(U_qrf <= t)
)

# statistical test
ks.test(U_qrf, "punif", min = a, max = b)







