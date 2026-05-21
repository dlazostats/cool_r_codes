# ============================================================
#  Double Bootstrap: Full Implementation
#  Statistic: ratio estimator (nonlinear, BCa-relevant)
#  Comparison: Percentile | BCa | Double Bootstrap
# ============================================================
set.seed(2024)

# ---- Data: ratio of means, skewed DGP ----
n  <- 60
x1 <- rgamma(n, shape=2, rate=1)
x2 <- x1 + rgamma(n, shape=1, rate=2)
true_ratio <- 2.5 / 2.0     # E[X2]/E[X1] = 2.5/2 = 1.25

stat_fn <- function(x1, x2) mean(x2) / mean(x1)
theta_hat <- stat_fn(x1, x2)
cat(sprintf("Observed ratio: %.4f | True ratio: %.4f\n\n", theta_hat, true_ratio))

# ============================================================
# 1. SINGLE BOOTSTRAP — outer replicates (shared by all methods)
# ============================================================
B1 <- 999
cat(sprintf("Outer bootstrap: B1 = %d\n", B1))

boot_outer <- replicate(B1, {
  idx <- sample(n, n, TRUE)
  stat_fn(x1[idx], x2[idx])
})

# ---- Percentile interval ----
perc_ci <- quantile(boot_outer, c(0.025, 0.975))

# ---- BCa interval ----
z_lo <- qnorm(0.025); z_hi <- qnorm(0.975)
z0   <- qnorm(pmax(1e-6, pmin(1-1e-6, mean(boot_outer < theta_hat))))
jk   <- sapply(1:n, function(i) stat_fn(x1[-i], x2[-i]))
jb   <- mean(jk)
a    <- sum((jb-jk)^3) / (6 * sum((jb-jk)^2)^1.5)
if(!is.finite(a)) a <- 0

a1   <- pmax(1/B1, pmin(0.499, pnorm(z0+(z0+z_lo)/(1-a*(z0+z_lo)))))
a2   <- pmax(0.501, pmin(1-1/B1, pnorm(z0+(z0+z_hi)/(1-a*(z0+z_hi)))))
bca_ci <- quantile(boot_outer, c(a1, a2))

cat(sprintf("z0_hat = %.4f | a_hat = %.4f\n", z0, a))
cat(sprintf("BCa corrected levels: alpha1=%.4f, alpha2=%.4f\n\n", a1, a2))

# ============================================================
# 2. DOUBLE BOOTSTRAP — inner replicates
# ============================================================
B2 <- 99
cat(sprintf("Inner bootstrap: B2 = %d  (total evals: %d)\n\n", B2, B1*B2))

alpha_nominal <- 0.05

# For each outer replicate, compute inner bootstrap coverage probability
cat("Computing inner bootstraps...\n")
p_hat <- numeric(B1)

for(b in 1:B1) {
  # Outer resample defines the "new population"
  idx_b  <- sample(n, n, TRUE)
  x1b    <- x1[idx_b]
  x2b    <- x2[idx_b]
  theta_b <- stat_fn(x1b, x2b)
  
  # Inner bootstrap from outer resample
  inner <- replicate(B2, {
    idx_bb <- sample(n, n, TRUE)
    stat_fn(x1b[idx_bb], x2b[idx_bb])
  })
  
  # p_hat[b] = fraction of inner replicates <= outer replicate
  p_hat[b] <- mean(inner <= theta_b)
}

# ---- Calibration: find alpha* such that coverage = 1-alpha ----
# The nominal interval [q_{alpha/2}, q_{1-alpha/2}] has true coverage:
# gamma(alpha) = mean(p_hat <= 1-alpha)
# We want gamma(alpha*) = 1 - alpha_nominal
# Search over alpha grid

alpha_grid <- seq(0.001, 0.30, by=0.001)
coverage_alpha <- sapply(alpha_grid, function(a_try) {
  mean(p_hat <= (1 - a_try))
})

# Find calibrated alpha: smallest alpha such that coverage >= 1-alpha_nominal
target   <- 1 - alpha_nominal
idx_cal  <- which(coverage_alpha >= target)

if(length(idx_cal) == 0) {
  cat("Warning: calibration failed — defaulting to percentile\n")
  alpha_cal <- alpha_nominal
} else {
  alpha_cal <- alpha_grid[min(idx_cal)]
}

cat(sprintf("Nominal alpha: %.3f | Calibrated alpha: %.4f\n\n", alpha_nominal, alpha_cal))

# Double bootstrap interval
db_ci <- quantile(boot_outer, c(alpha_cal/2, 1 - alpha_cal/2))

# ============================================================
# 3. Summary
# ============================================================
cat("=== INTERVAL COMPARISON ===\n")
cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n",
            "Percentile:", perc_ci[1], perc_ci[2], diff(perc_ci)))
cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n",
            "BCa:", bca_ci[1], bca_ci[2], diff(bca_ci)))
cat(sprintf("%-20s [%.4f, %.4f]  Width: %.4f\n",
            "Double Bootstrap:", db_ci[1], db_ci[2], diff(db_ci)))

# ============================================================
# 4. Coverage simulation
# ============================================================
cat("\nRunning coverage simulation (n_sim=500)...\n")

n_sim <- 500; B1s <- 299; B2s <- 49

cover <- c(perc=0, bca=0, db=0)

for(s in 1:n_sim) {
  x1s <- rgamma(n, 2, 1)
  x2s <- x1s + rgamma(n, 1, 2)
  ths <- stat_fn(x1s, x2s)
  
  bs  <- replicate(B1s, {
    i <- sample(n, n, TRUE)
    stat_fn(x1s[i], x2s[i])
  })
  
  # Percentile
  cip <- quantile(bs, c(0.025, 0.975))
  cover["perc"] <- cover["perc"] +
    (true_ratio >= cip[1] & true_ratio <= cip[2])
  
  # BCa
  z0s <- qnorm(pmax(1e-6, pmin(1-1e-6, mean(bs < ths))))
  jks <- sapply(1:n, function(i) stat_fn(x1s[-i], x2s[-i]))
  jbs <- mean(jks)
  as  <- sum((jbs-jks)^3) / (6*sum((jbs-jks)^2)^1.5)
  if(!is.finite(as)) as <- 0
  a1s <- pmax(1/B1s, pmin(0.499, pnorm(z0s+(z0s+z_lo)/(1-as*(z0s+z_lo)))))
  a2s <- pmax(0.501, pmin(1-1/B1s, pnorm(z0s+(z0s+z_hi)/(1-as*(z0s+z_hi)))))
  cib <- quantile(bs, c(a1s, a2s))
  cover["bca"] <- cover["bca"] +
    (true_ratio >= cib[1] & true_ratio <= cib[2])
  
  # Double bootstrap
  ph <- numeric(B1s)
  for(b in 1:B1s) {
    ib   <- sample(n, n, TRUE)
    x1b  <- x1s[ib]; x2b <- x2s[ib]
    thb  <- stat_fn(x1b, x2b)
    inn  <- replicate(B2s, {
      ibb <- sample(n, n, TRUE)
      stat_fn(x1b[ibb], x2b[ibb])
    })
    ph[b] <- mean(inn <= thb)
  }
  # Calibrate
  ag   <- seq(0.001, 0.30, by=0.002)
  cov_ag <- sapply(ag, function(a_try) mean(ph <= (1-a_try)))
  idx_c  <- which(cov_ag >= target)
  ac     <- if(length(idx_c)==0) alpha_nominal else ag[min(idx_c)]
  cid    <- quantile(bs, c(ac/2, 1-ac/2))
  cover["db"] <- cover["db"] +
    (true_ratio >= cid[1] & true_ratio <= cid[2])
}

{
  cat(sprintf("\n=== EMPIRICAL COVERAGE (n=%d, n_sim=%d, nominal=95%%) ===\n", n, n_sim))
  cat(sprintf("%-20s %.1f%%\n", "Percentile:", 100*cover["perc"]/n_sim))
  cat(sprintf("%-20s %.1f%%\n", "BCa:", 100*cover["bca"]/n_sim))
  cat(sprintf("%-20s %.1f%%\n", "Double Bootstrap:", 100*cover["db"]/n_sim))
}

