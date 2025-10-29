# Practicing MC Integration
#---------------------------
options(scipen=999)
# Exercise 1
f1<-function(t) sin(t)
a<-pi/3
b<-0

integrate(f1,a,b) # adaptative quadrature 
n<-10000          # basic MC
x<-runif(n,a,b)
fmc<-f1(x)
Y<-(b-a)*fmc
mean(Y)
var(Y)

# Exercise 2
set.seed(123)
f2<-function(x) exp(-x)
a<-0
b<-0.5

integrate(f2,a,b)

n<-10000
x<-runif(n,a,b)
fmc<-f2(x)
Y1<-(b-a)*fmc
mean(Y1)
var(Y1)/n

x<-rexp(n,rate=1)
xval<-x[x<=b]
fmc<-f2(xval)
Y2<-(b-a)*fmc
mean(Y2)
var(Y2)


# Claude example
#------------------
# Monte Carlo Integration: Comparing Uniform vs Exponential Sampling
# Integral: ∫₀^0.5 e^(-x) dx

set.seed(123)

# True value of the integral
true_value <- 1 - exp(-0.5)  # = -[e^(-x)]₀^0.5 = 1 - e^(-0.5)
cat(sprintf("True value: %.6f\n\n", true_value))

# ====== ESTIMATOR 1: Uniform Sampling ======
# θ̂₁ = (b-a) * (1/n) * Σ g(Uᵢ) where Uᵢ ~ Uniform(0, 0.5)

mc_uniform <- function(n, a = 0, b = 0.5) {
  # Sample from Uniform(0, 0.5)
  x <- runif(n, a, b)
  
  # Evaluate g(x) = e^(-x)
  g_x <- exp(-x)
  
  # MC estimate: (b-a) * mean(g(x))
  estimate <- (b - a) * mean(g_x)
  
  # For variance: Var(θ̂₁) = (b-a)² * Var(g(X)) / n
  var_g <- var(g_x)
  var_estimate <- (b - a)^2 * var_g / n
  
  return(list(
    estimate = estimate,
    var_estimate = var_estimate,
    g_values = g_x
  ))
}

# ====== ESTIMATOR 2: Exponential Sampling (Truncated) ======
# θ̂₂ = P(X ≤ 0.5) * (1/m) * Σ g(Xᵢ) where Xᵢ ~ Exp(λ), only keeping X ≤ 0.5

mc_exponential <- function(n, b = 0.5, lambda = 1) {
  # Sample from Exponential(lambda)
  x <- rexp(n, rate = lambda)
  
  # Keep only samples in [0, 0.5]
  x_valid <- x[x <= b]
  m <- length(x_valid)
  
  # Evaluate g(x) = e^(-x) on valid samples
  g_x <- exp(-x_valid)
  
  # Probability of being in [0, b]
  p_truncated <- pexp(b, rate = lambda)
  
  # MC estimate: P(X ≤ b) * mean(g(X) | X ≤ b)
  estimate <- p_truncated * mean(g_x)
  
  # Variance of estimate
  var_g <- var(g_x)
  var_estimate <- p_truncated^2 * var_g / m
  
  return(list(
    estimate = estimate,
    var_estimate = var_estimate,
    g_values = g_x,
    acceptance_rate = m/n
  ))
}

# ====== Run Simulations ======
n <- 10000
lambda <- 1  # Common choice

cat("ESTIMATOR 1: Uniform(0, 0.5) Sampling\n")
result_uniform <- mc_uniform(n)
cat(sprintf("Estimate (θ̂₁):        %.6f\n", result_uniform$estimate))
cat(sprintf("Variance of θ̂₁:       %.8f\n", result_uniform$var_estimate))
cat(sprintf("Standard Error:       %.6f\n", sqrt(result_uniform$var_estimate)))
cat(sprintf("Error from true:      %.6f\n\n", abs(result_uniform$estimate - true_value)))

cat("ESTIMATOR 2: Exponential(λ=1) Sampling (Truncated)\n")
result_exp <- mc_exponential(n, lambda = lambda)
cat(sprintf("Estimate (θ̂₂):        %.6f\n", result_exp$estimate))
cat(sprintf("Variance of θ̂₂:       %.8f\n", result_exp$var_estimate))
cat(sprintf("Standard Error:       %.6f\n", sqrt(result_exp$var_estimate)))
cat(sprintf("Acceptance Rate:      %.1f%%\n", result_exp$acceptance_rate * 100))
cat(sprintf("Error from true:      %.6f\n\n", abs(result_exp$estimate - true_value)))

# ====== Comparison ======
cat("COMPARISON\n")
cat(sprintf("Variance Ratio (Uniform/Exponential): %.4f\n", 
            result_uniform$var_estimate / result_exp$var_estimate))

if (result_uniform$var_estimate < result_exp$var_estimate) {
  cat("\n✓ Uniform sampling has LOWER variance (more efficient)\n")
} else {
  cat("\n✓ Exponential sampling has LOWER variance (more efficient)\n")
}

# ====== Repeated Simulations to Verify ======
cat("VERIFICATION: Multiple Runs\n")

n_simulations <- 1000
estimates_uniform <- numeric(n_simulations)
estimates_exp <- numeric(n_simulations)

for (i in 1:n_simulations) {
  estimates_uniform[i] <- mc_uniform(n)$estimate
  estimates_exp[i] <- mc_exponential(n, lambda = lambda)$estimate
}

var_uniform_empirical <- var(estimates_uniform)
var_exp_empirical <- var(estimates_exp)

cat(sprintf("Empirical Variance (Uniform):     %.8f\n", var_uniform_empirical))
cat(sprintf("Empirical Variance (Exponential): %.8f\n", var_exp_empirical))
cat(sprintf("Variance Ratio:                   %.4f\n\n", 
            var_uniform_empirical / var_exp_empirical))

# ====== Visualization ======
par(mfrow = c(2, 2))

# Histogram of g(x) values - Uniform
hist(result_uniform$g_values, 
     breaks = 50, 
     main = "g(X) with Uniform Sampling",
     xlab = "e^(-x)",
     col = "lightblue",
     xlim = c(0.6, 1))
abline(v = mean(result_uniform$g_values), col = "red", lwd = 2)

# Histogram of g(x) values - Exponential
hist(result_exp$g_values, 
     breaks = 50, 
     main = "g(X) with Exponential Sampling",
     xlab = "e^(-x)",
     col = "lightgreen",
     xlim = c(0.6, 1))
abline(v = mean(result_exp$g_values), col = "red", lwd = 2)

# Distribution of estimates - Uniform
hist(estimates_uniform, 
     breaks = 50, 
     main = "Distribution of θ̂₁ (Uniform)",
     xlab = "Estimate",
     col = "lightblue")
abline(v = true_value, col = "red", lwd = 2, lty = 2)

# Distribution of estimates - Exponential
hist(estimates_exp, 
     breaks = 50, 
     main = "Distribution of θ̂₂ (Exponential)",
     xlab = "Estimate",
     col = "lightgreen")
abline(v = true_value, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

# ====== WHY? Explanation ======
cat("WHY DOES ONE HAVE LOWER VARIANCE?\n")
cat("\nFor Uniform sampling:\n")
cat("  • Samples spread evenly across [0, 0.5]\n")
cat("  • g(x) = e^(-x) varies from 1 to e^(-0.5) ≈ 0.606\n")
cat(sprintf("  • Range of g(x): %.3f\n", 1 - exp(-0.5)))
cat(sprintf("  • Var(g(X)): %.6f\n", var(result_uniform$g_values)))

cat("\nFor Exponential sampling:\n")
cat("  • Samples concentrated near 0 (where e^(-x) ≈ 1)\n")
cat("  • Most g(x) values close to 1\n")
cat(sprintf("  • Var(g(X)): %.6f\n", var(result_exp$g_values)))

cat("\n→ Exponential sampling reduces variance because:\n")
cat("  1. It samples more where the integrand is larger\n")
cat("  2. This creates less variability in g(X) values\n")
cat("  3. Lower Var(g(X)) → Lower Var(θ̂)\n")

