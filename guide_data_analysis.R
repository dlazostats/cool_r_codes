library(ggplot2)

# Define the nonlinear function (exponential model)
nonlinear_model <- function(theta, x) {
  theta[1] * exp(theta[2] * x)
}

# Define the Sum of Squared Errors function
sse <- function(theta, x, y) {
  sum((y - nonlinear_model(theta, x))^2)
}

# Define Gradient of SSE w.r.t theta (computed numerically)
gradient_sse <- function(theta, x, y) {
  n <- length(y)
  residuals <- y - nonlinear_model(theta, x)
  
  # Partial derivative w.r.t theta_1
  grad_1 <- -2 * sum(residuals * exp(theta[2] * x))
  
  # Partial derivative w.r.t theta_2
  grad_2 <- -2 * sum(residuals * theta[1] * x * exp(theta[2] * x))
  
  return(c(grad_1, grad_2))
}

# Generate synthetic data
set.seed(123)
x <- seq(0, 1, length.out = 100)
true_theta <- c(2, 0.3)
y <- nonlinear_model(true_theta, x) + rnorm(length(x), sd = 0.5)

gradient_descent <-function(theta_init,x,y,alpha = 0.01,tol = 1e-5,max_iter = 500) {
    theta <- theta_init
    sse_values <- numeric(max_iter)
    for (j in 1:max_iter) {
      grad <- gradient_sse(theta, x, y)
      # Check for NaN or Inf values in the gradient (prevents divergence)
      if (any(is.na(grad)) || any(is.infinite(grad))) {
        cat("Numerical instability detected at iteration",j,"\n")
        break
      }
      # Update step
      theta_new <- theta - alpha*grad
      sse_values[j] <- sse(theta_new, x, y)
      # Check for convergence
      if (!is.finite(sse_values[j])) {
        cat("Divergence detected at iteration", j, "\n")
        break
      }
      if (sum(abs(theta_new - theta)) < tol) {
        cat("Converged in", j, "iterations.\n")
        return(list(theta = theta_new, sse_values = sse_values[1:j]))
      }
      theta <- theta_new
    }
    return(list(theta = theta, sse_values = sse_values))
  }

# Run Gradient Descent with a Safe Implementation
theta_init <- c(1.5, 0.1)  # Initial guess
alpha <- 1e-4            # Learning rate
result <- gradient_descent(theta_init, x, y, alpha)
result$theta


opt_result <- optim(par = theta_init,
                    fn = sse,
                    x = x,
                    y = y,
                    method = "BFGS")

df <- data.frame(x = x, y = y)
nls_model <- nls(y ~ theta1 * exp(theta2 * x),
                 data = df,
                 start = list(theta1 = 1, theta2 = 0.1))
coef(nls_model)

