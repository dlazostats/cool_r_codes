# ============================================================
# 1. BASIC DIRICHLET DRAWS
# ============================================================

# 5 observations, flat prior (alpha = 1)
cat("--- Two Dirichlet draws with 5 observations ---\n")
draw1 <- rdirichlet(1, alpha = rep(1, 5))
draw2 <- rdirichlet(1, alpha = rep(1, 5))

print(round(draw1, 3))
print(round(draw2, 3))
cat("Sum of draw 1:", sum(draw1), "\n")
cat("Sum of draw 2:", sum(draw2), "\n")

# ============================================================
# 2. EFFECT OF ALPHA PARAMETER
# ============================================================

cat("\n--- Effect of alpha on weights ---\n")

# flat: weights spread freely
flat    <- rdirichlet(5, alpha = c(1, 1, 1))

# concentrated: weights close to equal (1/3)
conc    <- rdirichlet(5, alpha = c(10, 10, 10))

# skewed: first observation gets more weight
skewed  <- rdirichlet(5, alpha = c(5, 1, 1))

cat("Flat alpha=1:\n");     print(round(flat, 3))
cat("Concentrated alpha=10:\n"); print(round(conc, 3))
cat("Skewed alpha=c(5,1,1):\n"); print(round(skewed, 3))

# ============================================================
# 3. VISUALIZE ALPHA EFFECT (1000 draws, 3 observations)
# ============================================================

n_draws <- 1000

w_flat  <- rdirichlet(n_draws, alpha = c(1, 1, 1))
w_conc  <- rdirichlet(n_draws, alpha = c(10, 10, 10))
w_skew  <- rdirichlet(n_draws, alpha = c(5, 1, 1))

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

hist(w_flat[, 1],
     main  = "alpha = 1 (flat)",
     xlab  = "Weight on obs 1",
     col   = "steelblue",
     border = "white",
     xlim  = c(0, 1),
     breaks = 30)
abline(v = 1/3, col = "red", lwd = 2, lty = 2)

hist(w_conc[, 1],
     main  = "alpha = 10 (concentrated)",
     xlab  = "Weight on obs 1",
     col   = "darkorange",
     border = "white",
     xlim  = c(0, 1),
     breaks = 30)
abline(v = 1/3, col = "red", lwd = 2, lty = 2)

hist(w_skew[, 1],
     main  = "alpha = c(5,1,1) (skewed)",
     xlab  = "Weight on obs 1",
     col   = "forestgreen",
     border = "white",
     xlim  = c(0, 1),
     breaks = 30)
abline(v = 5/7, col = "red", lwd = 2, lty = 2)  # expected = 5/(5+1+1)

# ============================================================
# 4. MANUAL BAYESIAN BOOTSTRAP (replicating bayesboot)
# ============================================================

# Toy example: 8 observations
set.seed(42)
dif_toy <- c(0.12, -0.05, 0.18, 0.09, -0.02, 0.15, 0.11, 0.08)
n       <- length(dif_toy)
R       <- 10000

# Store results
posterior_means <- numeric(R)

for (i in 1:R) {
  w <- rdirichlet(1, alpha = rep(1, n))   # flat prior
  posterior_means[i] <- sum(w * dif_toy)  # weighted mean
}

cat("\n--- Manual Bayesian Bootstrap Results ---\n")
cat("Posterior mean:  ", round(mean(posterior_means), 4), "\n")
cat("Posterior median:", round(median(posterior_means), 4), "\n")
cat("95% HDI:         ", round(quantile(posterior_means, 0.025), 4),
    "to", round(quantile(posterior_means, 0.975), 4), "\n")
cat("P(mean > 0):     ", round(mean(posterior_means > 0), 3), "\n")

# ============================================================
# 5. COMPARE MANUAL vs bayesboot
# ============================================================

library(bayesboot)

bb <- bayesboot(dif_toy, mean, R = R)

cat("\n--- bayesboot Results (should match manual) ---\n")
summary(bb)
