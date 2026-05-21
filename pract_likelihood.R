set.seed(42)

# Example 1

x <- rnorm(100, mean = 5, sd = 2)  # true μ=5, σ=2

neg_loglik <- function(params, data) {
  mu    <- params[1]
  sigma <- params[2]
  
  if (sigma <= 0) return(Inf)  # sigma must be positive
  
  -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

init <- c(mu = 0, sigma = 1) 

fit <- optim(
  par    = init,
  fn     = neg_loglik,
  data   = x,
  method = "Nelder-Mead"
)

fit$par 

{
  cat("MLE via optim: mu =", round(fit$par[1], 4), " sigma =", round(fit$par[2], 4), "\n")
  cat("Analytic MLE:  mu =", round(mean(x), 4),    " sigma =", round(sd(x) * sqrt((100-1)/100), 4), "\n")
  # Note: analytic MLE of sigma uses n in denominator, not n-1  
}

# Example 2
set.seed(1) 
x <- rexp(80, rate = 2)

neg_loglik <- function(lambda, data) {
  if (lambda <= 0) return(Inf)
  -sum(dexp(data, rate = lambda, log = TRUE))
}

fit <- optim(par = 1, 
             fn = neg_loglik, 
             data = x,
             method = "Brent", 
             lower = 0.01, upper = 20)
fit$par          
1/mean(x) 

# Example 2
set.seed(7)
x <- rpois(60, lambda = 4.5)

neg_loglik <- function(lambda, data) {
  if (lambda <= 0) return(Inf)
  -sum(dpois(data, lambda = lambda, log = TRUE))
}

fit <- optim(par = 1, 
             fn = neg_loglik, 
             data = x,
             method = "Brent", lower = 0.01, upper = 20)
fit$par      # MLE ≈ mean(x)
mean(x)

# Fisher information
#---------------------
set.seed(1) 
x <- rexp(80, rate = 2)

neg_loglik <- function(lambda, data) {
  if (lambda <= 0) return(Inf)
  -sum(dexp(data, rate = lambda, log = TRUE))
}

fit <- optim(par = 1, 
             fn = neg_loglik, 
             data = x,
             method = "Brent", 
             hessian=T,
             lower = 0.01, upper = 20)
fit$hessian

# standar error of estimates
fish_info <- solve(fit$hessian)   # invert the Hessian
se <- sqrt(diag(fish_info))  

fit$par
z <- qnorm(0.975) 
c(fit$par-z*se,fit$par+z*se)

# check if is a true maximum 
eigen(fit$hessian)$values  # all positive → you're at a minimum of -loglik (= maximum of loglik)

# verify convergence
all(eigen(fit$hessian)$values > 0)









