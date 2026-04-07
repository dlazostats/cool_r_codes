set.seed(42)
x <- c(rnorm(150, mean = 2,  sd = 0.8),
       rnorm(150, mean = 7,  sd = 1.2))

K     <- 2
pi_k  <- c(0.5, 0.5)          # mixing weights
mu_k  <- c(1, 5)               # means (deliberately off)
sig_k <- c(1, 1) 

log_lik_history <- numeric()

em_gmm <- function(x, K, pi_k, mu_k, sig_k, tol = 1e-6, max_iter = 500) {
  n <- length(x)
  
  for (iter in seq_len(max_iter)) {
    
    # ── E-step: compute responsibilities r[i,k] ──────────────────────────
    r <- matrix(0, nrow = n, ncol = K)
    for (k in 1:K) {
      r[, k] <- pi_k[k] * dnorm(x, mean = mu_k[k], sd = sig_k[k])
    }
    r <- r / rowSums(r)   # normalise rows to sum to 1
    
    # ── M-step: update parameters ─────────────────────────────────────────
    Nk    <- colSums(r)
    pi_k  <- Nk / n
    mu_k  <- colSums(r * x) / Nk
    sig_k <- sqrt(colSums(r * outer(x, mu_k, "-")^2) / Nk)
    
    # ── Log-likelihood (for convergence check) ────────────────────────────
    ll <- sum(log(rowSums(sapply(1:K, function(k)
      pi_k[k] * dnorm(x, mu_k[k], sig_k[k])))))
    log_lik_history <<- c(log_lik_history, ll)
    
    if (iter > 1 && abs(ll - tail(log_lik_history, 2)[1]) < tol) {
      cat("Converged in", iter, "iterations\n")
      break
    }
  }
  
  list(pi = pi_k, mu = mu_k, sigma = sig_k, loglik = log_lik_history)
}

result <- em_gmm(x, K, pi_k, mu_k, sig_k)
#> Converged in 43 iterations

cat("Estimated means: ",  round(result$mu,  3), "\n")
cat("Estimated sigmas:", round(result$sigma, 3), "\n")
cat("Mixing weights:  ", round(result$pi,   3), "\n")
#> Estimated means:  2.023  7.041
#> Estimated sigmas: 0.797 1.189
#> Mixing weights:   0.500 0.500

# ── Visualise ──────────────────────────────────────────────────────────────
par(mfrow = c(1, 2))

# Fitted density overlay
hist(x, breaks = 40, freq = FALSE, main = "GMM fit",
     xlab = "x", col = "grey90", border = "white")
xg <- seq(min(x), max(x), length.out = 500)
fitted_density <- rowSums(sapply(1:K, function(k)
  result$pi[k] * dnorm(xg, result$mu[k], result$sigma[k])))
lines(xg, fitted_density, col = "steelblue", lwd = 2)

# Log-likelihood over iterations (should be monotonically non-decreasing)
plot(result$loglik, type = "l", col = "coral", lwd = 2,
     main = "Log-likelihood per iteration",
     xlab = "Iteration", ylab = "Log-likelihood")
