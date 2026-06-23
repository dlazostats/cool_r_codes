# ============================================================
#  PERMUTATION TESTS IN R
#  1. Classic permutation test (equal variances assumed)
#  2. Studentized permutation test (unequal variances)
# ============================================================

set.seed(42)

# ── Simulated data ───────────────────────────────────────────
# Group A: 20 students taught with Method A
# Group B: 18 students taught with Method B
group_a <- c(72, 85, 78, 90, 65, 88, 76, 82, 70, 95,
             80, 74, 68, 91, 77, 83, 69, 87, 73, 84)

group_b <- c(65, 70, 58, 75, 62, 80, 55, 72, 60, 68,
             74, 63, 57, 78, 66, 71, 59, 76)

{
  cat("=== Descriptive Statistics ===\n")
  cat(sprintf("Group A: n=%d, mean=%.2f, sd=%.2f\n",
              length(group_a), mean(group_a), sd(group_a)))
  cat(sprintf("Group B: n=%d, mean=%.2f, sd=%.2f\n",
              length(group_b), mean(group_b), sd(group_b)))
  cat(sprintf("Observed difference (A - B): %.4f\n\n",
              mean(group_a) - mean(group_b)))  
}


# ============================================================
#  1. CLASSIC PERMUTATION TEST
#     Test statistic: difference of means
#     Assumption: same distribution shape (including variance)
# ============================================================

classic_permutation_test <- function(x, y, n_perm = 10000) {
  
  # Observed test statistic
  obs_stat <- mean(x) - mean(y)
  n_x <- length(x)
  
  # Pool all observations
  pooled <- c(x, y)
  n_total <- length(pooled)
  
  # Generate the null distribution by permuting
  perm_stats <- replicate(n_perm, {
    shuffled  <- sample(pooled)          # random shuffle
    perm_x    <- shuffled[1:n_x]        # first n_x → "group A"
    perm_y    <- shuffled[(n_x+1):n_total]
    mean(perm_x) - mean(perm_y)         # test statistic
  })
  
  # Two-sided p-value
  p_value <- mean(abs(perm_stats) >= abs(obs_stat))
  
  list(obs_stat = obs_stat,
       perm_stats = perm_stats,
       p_value = p_value,
       n_perm = n_perm)
}

res_classic <- classic_permutation_test(group_a, group_b, n_perm = 10000)

{
  cat("=== 1. Classic Permutation Test ===\n")
  cat(sprintf("Observed difference of means : %.4f\n", res_classic$obs_stat))
  cat(sprintf("Two-sided p-value            : %.4f\n", res_classic$p_value))
  if (res_classic$p_value < 0.05) {
    cat("→ Reject H0 at α = 0.05: the means differ significantly.\n\n")
  } else {
    cat("→ Fail to reject H0 at α = 0.05.\n\n")
  }
}


# ============================================================
#  2. STUDENTIZED PERMUTATION TEST  (Welch-style)
#     Test statistic: t-statistic with separate variance estimates
#     Better when groups have UNEQUAL variances or sizes
# ============================================================

# Helper: Welch t-statistic
welch_t <- function(x, y) {
  (mean(x) - mean(y)) / sqrt(var(x)/length(x) + var(y)/length(y))
}

studentized_permutation_test <- function(x, y, n_perm = 10000) {
  
  obs_stat <- welch_t(x, y)
  n_x      <- length(x)
  pooled   <- c(x, y)
  n_total  <- length(pooled)
  
  perm_stats <- replicate(n_perm, {
    shuffled <- sample(pooled)
    perm_x   <- shuffled[1:n_x]
    perm_y   <- shuffled[(n_x+1):n_total]
    welch_t(perm_x, perm_y)
  })
  
  p_value <- mean(abs(perm_stats) >= abs(obs_stat))
  
  list(obs_stat = obs_stat,
       perm_stats = perm_stats,
       p_value = p_value,
       n_perm = n_perm)
}

res_stud <- studentized_permutation_test(group_a, group_b, n_perm = 10000)

{
  cat("=== 2. Studentized Permutation Test (Unequal Variances) ===\n")
  cat(sprintf("Observed Welch t-statistic : %.4f\n", res_stud$obs_stat))
  cat(sprintf("Two-sided p-value          : %.4f\n", res_stud$p_value))
  if (res_stud$p_value < 0.05) {
    cat("→ Reject H0 at α = 0.05: the means differ significantly.\n\n")
  } else {
    cat("→ Fail to reject H0 at α = 0.05.\n\n")
  }  
}



# ── Comparison with parametric t-test ───────────────────────
cat("=== Parametric Reference (Welch t-test) ===\n")
print(t.test(group_a, group_b))


# ============================================================
#  VISUALIZE NULL DISTRIBUTIONS
# ============================================================

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Plot 1 – Classic
hist(res_classic$perm_stats,
     breaks = 50,
     col    = "#AED6F1",
     border = "white",
     main   = "Classic Permutation\nNull Distribution",
     xlab   = "Difference of Means",
     freq   = FALSE)
abline(v =  res_classic$obs_stat, col = "red",  lwd = 2, lty = 2)
abline(v = -res_classic$obs_stat, col = "red",  lwd = 2, lty = 2)
legend("topright",
       legend = sprintf("p = %.4f", res_classic$p_value),
       bty = "n", text.col = "red")

# Plot 2 – Studentized
hist(res_stud$perm_stats,
     breaks = 50,
     col    = "#A9DFBF",
     border = "white",
     main   = "Studentized Permutation\nNull Distribution",
     xlab   = "Welch t-statistic",
     freq   = FALSE)
abline(v =  res_stud$obs_stat, col = "darkred", lwd = 2, lty = 2)
abline(v = -res_stud$obs_stat, col = "darkred", lwd = 2, lty = 2)
legend("topright",
       legend = sprintf("p = %.4f", res_stud$p_value),
       bty = "n", text.col = "darkred")

par(mfrow = c(1, 1))
