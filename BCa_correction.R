# Better BCa demo using a cleaner statistic: the median on skewed data
# where BCa corrections are meaningful and stable
set.seed(42)

n <- 50
x <- rexp(n, rate = 0.5)   # Exponential: skewed, median = log(2)/0.5 = 1.386
true_median <- log(2) / 0.5

theta_hat <- median(x)
cat(sprintf("True median: %.4f | Observed median: %.4f\n\n", true_median, theta_hat))

B <- 5000

boot_med <- replicate(B, median(sample(x, n, replace=TRUE)))

# Percentile
perc_ci <- quantile(boot_med, c(0.025, 0.975))

# BCa
z0_hat <- qnorm(mean(boot_med < theta_hat))
jack_med <- sapply(1:n, function(i) median(x[-i]))
jbar <- mean(jack_med)
num  <- sum((jbar - jack_med)^3)
den  <- 6 * (sum((jbar - jack_med)^2))^(3/2)
a_hat <- num / den

{
  cat(sprintf("z0_hat = %+.4f  (%.1f%% of bootstrap reps below theta_hat)\n",
              z0_hat, 100*mean(boot_med < theta_hat)))
  cat(sprintf("a_hat  = %+.4f  (skewness of jackknife distribution)\n\n", a_hat))  
}

z_lo <- qnorm(0.025); z_hi <- qnorm(0.975)
alpha1 <- pnorm(z0_hat + (z0_hat + z_lo)/(1 - a_hat*(z0_hat + z_lo)))
alpha2 <- pnorm(z0_hat + (z0_hat + z_hi)/(1 - a_hat*(z0_hat + z_hi)))
bca_ci <- quantile(boot_med, c(alpha1, alpha2))

# Normal approximation via CLT for median
se_med <- 1/(2*dexp(theta_hat, 0.5)*sqrt(n))
norm_ci <- theta_hat + c(-1,1)*1.96*se_med

{
  cat("=== INTERVAL COMPARISON ===\n")
  cat(sprintf("True median:  %.4f\n", true_median))
  cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n", "Normal approx:", norm_ci[1], norm_ci[2], diff(norm_ci)))
  cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n", "Percentile:", perc_ci[1], perc_ci[2], diff(perc_ci)))
  cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n", "BCa:", bca_ci[1], bca_ci[2], diff(bca_ci)))
  cat(sprintf("\nCorrected quantiles: alpha1=%.4f (%.2f%%), alpha2=%.4f (%.2f%%)\n",
              alpha1, 100*alpha1, alpha2, 100*alpha2))  
}

# Coverage simulation
cat("\nRunning coverage simulation...\n")
n_sim <- 2000; B_sim <- 300
cover <- c(norm=0, perc=0, bca=0)

for(s in 1:n_sim){
  xs  <- rexp(n, 0.5)
  th  <- median(xs)
  bs  <- replicate(B_sim, median(sample(xs, n, replace=TRUE)))
  
  # Normal
  se_s   <- 1/(2*dexp(th, 0.5)*sqrt(n))
  ci_n   <- th + c(-1,1)*1.96*se_s
  cover["norm"] <- cover["norm"] + (true_median >= ci_n[1] & true_median <= ci_n[2])
  
  # Percentile
  ci_p   <- quantile(bs, c(0.025, 0.975))
  cover["perc"] <- cover["perc"] + (true_median >= ci_p[1] & true_median <= ci_p[2])
  
  # BCa
  z0_s   <- qnorm(pmax(0.001, pmin(0.999, mean(bs < th))))
  jk_s   <- sapply(1:n, function(i) median(xs[-i]))
  jb_s   <- mean(jk_s)
  a_s    <- sum((jb_s-jk_s)^3) / (6*sum((jb_s-jk_s)^2)^(3/2))
  if(!is.finite(a_s)) a_s <- 0
  a1_s   <- pnorm(z0_s + (z0_s+z_lo)/(1-a_s*(z0_s+z_lo)))
  a2_s   <- pnorm(z0_s + (z0_s+z_hi)/(1-a_s*(z0_s+z_hi)))
  a1_s   <- pmax(0.001, pmin(0.499, a1_s))
  a2_s   <- pmax(0.501, pmin(0.999, a2_s))
  ci_b   <- quantile(bs, c(a1_s, a2_s))
  cover["bca"] <- cover["bca"] + (true_median >= ci_b[1] & true_median <= ci_b[2])
}

{
  cat(sprintf("\n=== EMPIRICAL COVERAGE (n=%d, n_sim=%d, nominal=95%%) ===\n", n, n_sim))
  cat(sprintf("%-20s %.1f%%\n", "Normal approx:", 100*cover["norm"]/n_sim))
  cat(sprintf("%-20s %.1f%%\n", "Percentile:", 100*cover["perc"]/n_sim))
  cat(sprintf("%-20s %.1f%%\n", "BCa:", 100*cover["bca"]/n_sim))
}

