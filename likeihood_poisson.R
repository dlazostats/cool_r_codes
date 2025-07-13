# Ho: is true theta1=theta2=theta
#--------------------------------
log_like<-function(theta,y){
    -sum(y*log(theta) - theta - log(factorial(y))) #lgamma(y + 1)
}
y_ho <- c(0, 1, 1, 0, 2, 3, 0, 1, 1, 1, 1, 2, 0, 1, 3, 0, 1, 2, 1, 3, 3, 4, 1, 3, 2, 0,
  2, 0, 3, 0, 0, 1, 1, 1, 1, 0, 0, 2, 2, 0, 1, 2, 0, 0, 1, 1, 1, 0, 2)
theta_opt <- optim(par = 1, 
                   fn = log_like, 
                   y = y_ho, method = "Brent", 
                   lower = 0.001, upper = 10)
theta_opt$par
-log_like(theta_opt$par,y_ho)



# Data for both groups
group1 <- c(0, 1, 1, 0, 2, 3, 0, 1, 1, 1, 1, 2, 0, 1, 3, 0, 1, 2, 1, 3, 3, 4, 1, 3, 2, 0)
group2 <- c(2, 0, 3, 0, 0, 1, 1, 1, 1, 0, 0, 2, 2, 0, 1, 2, 0, 0, 1, 1, 1, 0, 2)

# Negative log-likelihood function for Poisson
neg_log_likelihood <- function(theta, y) {
  if (theta <= 0) return(Inf)
  -sum(y * log(theta) - theta - lgamma(y + 1))
}

# Optimize for Group 1
opt1 <- optim(par = 1, fn = neg_log_likelihood, y = group1, method = "Brent", lower = 0.001, upper = 10)

# Optimize for Group 2
opt2 <- optim(par = 1, fn = neg_log_likelihood, y = group2, method = "Brent", lower = 0.001, upper = 10)

# Print results
cat("Optimal theta for Group 1:", opt1$par, "\n")
cat("Optimal theta for Group 2:", opt2$par, "\n")
ag<-read.delim("clipboard")


# Full 'cron' data (one group)
cron <- c(
  0, 1, 1, 0, 2, 3, 0, 1, 1, 1, 1, 2, 0, 1, 3, 0, 1, 2, 1, 3, 3, 4, 1, 3, 2, 0,
  2, 0, 3, 0, 0, 1, 1, 1, 1, 0, 0, 2, 2, 0, 1, 2, 0, 0, 1, 1, 1, 0, 2
)

# Negative log-likelihood function for Poisson
neg_log_likelihood <- function(theta, y) {
  if (theta <= 0) return(Inf)
  -sum(y * log(theta) - theta - lgamma(y + 1))
}

# Run optimization to estimate theta
result <- optim(par = 1, fn = neg_log_likelihood, y = cron, method = "Brent", lower = 0.001, upper = 10)

# Print the optimal theta
cat("Optimal theta (Poisson MLE) for the single group:", result$par, "\n")
