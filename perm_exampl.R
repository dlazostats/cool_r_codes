# --- Permutation test in R ---

# Your data
group_A <- c(0.29, 13.82, 5.27, 11.45, 8.98, 13.57, 13.55, 8.44, 3.78, 9.92, 8.48, 13.47, 11.64, 9.85, 5.66)
group_B <- c(8.04, 9.91, 12.59, 13.81, 9.92, 13.54, 16.82, 10.51, 11.09, 4.62, 10.55, 16.05, 10.78, 21.36, 17.8)

# Observed difference in means
obs_diff <- mean(group_B) - mean(group_A)
cat("Observed diff:", round(obs_diff, 3), "\n")

# Permutation loop
set.seed(42)
n_perms  <- 1000
combined <- c(group_A, group_B)
n        <- length(group_A)

null_dist <- replicate(n_perms, {
  shuffled <- sample(combined)
  mean(shuffled[(n+1):(2*n)]) - mean(shuffled[1:n])
})

# Two-tailed p-value
p_val <- mean(abs(null_dist) >= abs(obs_diff))
cat("p-value:", round(p_val, 4), "\n")

# Plot null distribution
hist(null_dist, breaks = 40, col = "lightblue",
     main = "Null Distribution (Permuted Differences)",
     xlab = "Difference in means")
abline(v = obs_diff,  col = "red",  lwd = 2, lty = 2)
abline(v = -obs_diff, col = "red",  lwd = 2, lty = 2)
legend("topright", legend = "Observed diff",
       col = "red", lty = 2, lwd = 2)
