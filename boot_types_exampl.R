library(bayesboot)
library(boot)

# basic bootstrap
set.seed(1)
x <- rnorm(50, mean = 5)
B <- 2000

theta <- replicate(B, mean(sample(x, replace = TRUE)))
hist(theta,breaks="FD")
sd(theta)                       # bootstrap standard error
quantile(theta, c(.025, .975))  # percentile CI

# bayesian bootstrap
bayes_boot <- function(x, B = 2000, stat = weighted.mean) {
  n <- length(x)
  replicate(B, {
    w <- rexp(n)      # Gamma(1) draws
    w <- w / sum(w)   # normalize -> Dirichlet(1,...,1)
    stat(x, w)
  })
}
tb <- bayes_boot(x)
hist(tb,breaks="FD")
quantile(tb, c(.025, .975))

b <- bayesboot(x, weighted.mean)   # or supply a function taking (data, weights)
hist(b$V1,breaks="FD")
summary(b)

# weighted variance
wvar <- function(x, w) {
  m <- weighted.mean(x, w)
  sum(w * (x - m)^2)
}
b_var <- bayesboot(x, wvar, use.weights = TRUE)
hist(b_var$V1,breaks="FD")

# Smooth bootstrap: adding a bit of kernel noise, so you draw from a smoothed (kernel-density) version 
smooth_boot_vc <- function(x, B = 2000, h = bw.nrd(x)) {
  n <- length(x); m <- mean(x); s2 <- var(x)
  replicate(B, {
    i  <- sample(n, replace = TRUE)
    e  <- rnorm(n, 0, h)
    xs <- m + (x[i] - m + e) / sqrt(1 + h^2 / s2)
    mean(xs)
  })
}
theta <- replicate(B, mean(smooth_boot_vc(x)))
hist(theta)

# time series bootstrap
meanfun <- function(d) mean(d)
tsboot(x, meanfun, R = 2000, l = 10, sim = "geom")  #
