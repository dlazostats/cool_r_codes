library(gtools) 
set.seed(42)
rdirichlet(1, alpha = rep(1, 5))

# Simulate 1000 draws for 3 observations
w1 <- rdirichlet(1000, alpha = c(1, 1, 1))    # flat
w2 <- rdirichlet(1000, alpha = c(10, 10, 10)) # concentrated
w3 <- rdirichlet(1000, alpha = c(5, 1, 1))    # first obs favored

hist(w1[,1], main="alpha=1 (flat)",        xlab="weight on obs 1")
hist(w2[,1], main="alpha=10 (concentrated)",xlab="weight on obs 1")
hist(w3[,1], main="alpha=5,1,1 (skewed)",  xlab="weight on obs 1")

# How bayesboot uses dirichlet
set.seed(2121)
dif<-rnorm(200,-1.3,0.2)

# For your data with n observations, each iteration:
n <- length(dif)

# Step 1: draw random weights
w <- rdirichlet(1, alpha = rep(1, n))   # flat prior → equal alpha

# Step 2: compute weighted statistic
weighted_mean <- sum(w*dif)
