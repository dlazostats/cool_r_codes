# ============================================================
# Bayesian Bootstrap Example
# Comparing two teaching methods (A vs B)
# ============================================================

library(gtools)
library(bayesboot)
library(bayestestR)

set.seed(123)

# ============================================================
# 1. SIMULATE DATA
# Two classrooms, different teaching methods
# Scores out of 100
# ============================================================

method_A <- c(72, 85, 78, 90, 65, 88, 76, 82, 79, 91,
              70, 84, 77, 89, 68, 87, 75, 83, 80, 92)

method_B <- c(68, 79, 74, 85, 61, 83, 71, 77, 75, 86,
              65, 80, 72, 84, 63, 82, 70, 78, 76, 88)

cat("=== Descriptive Statistics ===\n")
cat(sprintf("Method A: mean = %.2f, sd = %.2f, n = %d\n",
            mean(method_A), sd(method_A), length(method_A)))
cat(sprintf("Method B: mean = %.2f, sd = %.2f, n = %d\n",
            mean(method_B), sd(method_B), length(method_B)))
cat(sprintf("Observed difference (A - B): %.2f\n",
            mean(method_A) - mean(method_B)))

# ============================================================
# 2. BAYESIAN BOOTSTRAP ON EACH GROUP SEPARATELY
# ============================================================

R <- 10000

bb_A <- bayesboot(method_A, mean, R = R)
bb_B <- bayesboot(method_B, mean, R = R)

# Posterior of the difference
posterior_diff <- bb_A$V1 - bb_B$V1

cat("\n=== Posterior of Mean Difference (A - B) ===\n")
cat(sprintf("Posterior mean:   %.3f\n", mean(posterior_diff)))
cat(sprintf("Posterior median: %.3f\n", median(posterior_diff)))

# ============================================================
# 3. PROBABILITY STATEMENTS
# ============================================================

p_A_better  <- mean(posterior_diff > 0)
p_B_better  <- mean(posterior_diff < 0)
p_large_gap <- mean(posterior_diff > 5)   # A beats B by more than 5 pts

cat("\n=== Probability Statements ===\n")
cat(sprintf("P(A > B | data):              %.3f\n", p_A_better))
cat(sprintf("P(B > A | data):              %.3f\n", p_B_better))
cat(sprintf("P(A beats B by > 5pts | data): %.3f\n", p_large_gap))

# ============================================================
# 4. ROPE ANALYSIS
# ============================================================

# On a 100-point scale, differences < 2 points are negligible
rope_low  <- -2
rope_high <-  2

describe_posterior(posterior_diff,
                   ci        = 0.95,
                   ci_method = "HDI",
                   rope_range = c(rope_low, rope_high))

# ============================================================
# 5. BAYESIAN BOOTSTRAP ON OTHER STATISTICS
# (not just the mean — this is where Bayesian bootstrap shines)
# ============================================================

# Posterior of the MEDIAN difference
bb_median_A <- bayesboot(method_A, median, R = R)
bb_median_B <- bayesboot(method_B, median, R = R)
posterior_median_diff <- bb_median_A$V1 - bb_median_B$V1

# Posterior of the SD difference (variability comparison)
bb_sd_A <- bayesboot(method_A, sd, R = R)
bb_sd_B <- bayesboot(method_B, sd, R = R)
posterior_sd_diff <- bb_sd_A$V1 - bb_sd_B$V1

# Posterior of the 90th percentile difference
# (how do TOP students compare?)
p90 <- function(x) quantile(x, 0.90)
bb_p90_A <- bayesboot(method_A, p90, R = R)
bb_p90_B <- bayesboot(method_B, p90, R = R)
posterior_p90_diff <- bb_p90_A$V1 - bb_p90_B$V1

cat("\n=== Beyond the Mean ===\n")
cat(sprintf("P(median_A > median_B | data):     %.3f\n",
            mean(posterior_median_diff > 0)))
cat(sprintf("P(sd_A > sd_B | data):             %.3f\n",
            mean(posterior_sd_diff > 0)))
cat(sprintf("P(90th pct A > 90th pct B | data): %.3f\n",
            mean(posterior_p90_diff > 0)))

# ============================================================
# 6. VISUALIZATIONS
# ============================================================

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# --- Plot 1: Raw data ---
boxplot(method_A, method_B,
        names  = c("Method A", "Method B"),
        col    = c("steelblue", "tomato"),
        main   = "Test Scores by Method",
        ylab   = "Score",
        border = "gray30")
points(1, mean(method_A), pch = 18, col = "darkblue",  cex = 2)
points(2, mean(method_B), pch = 18, col = "darkred",   cex = 2)

# --- Plot 2: Posterior distributions of each mean ---
xlim_range <- range(c(bb_A$V1, bb_B$V1))
hist(bb_A$V1,
     main   = "Posterior of Each Mean",
     xlab   = "Score",
     col    = adjustcolor("steelblue", alpha.f = 0.6),
     border = "white",
     breaks = 50,
     xlim   = xlim_range,
     freq   = FALSE)
hist(bb_B$V1,
     col    = adjustcolor("tomato", alpha.f = 0.6),
     border = "white",
     breaks = 50,
     add    = TRUE,
     freq   = FALSE)
legend("topleft",
       legend = c("Method A", "Method B"),
       fill   = c(adjustcolor("steelblue", 0.6),
                  adjustcolor("tomato", 0.6)),
       border = "white", bty = "n")

# --- Plot 3: Posterior of mean difference ---
hist(posterior_diff,
     main   = "Posterior: Mean Difference (A - B)",
     xlab   = "Score difference",
     col    = "mediumpurple",
     border = "white",
     breaks = 50,
     freq   = FALSE)
abline(v = 0,                            col = "red",    lwd = 2, lty = 2)
abline(v = mean(posterior_diff),         col = "black",  lwd = 2)
abline(v = quantile(posterior_diff,
                    c(0.025, 0.975)),    col = "orange", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Zero", "Posterior mean", "95% HDI"),
       col    = c("red", "black", "orange"),
       lwd    = 2, lty   = c(2, 1, 2), bty = "n")

# --- Plot 4: Posterior of median difference ---
hist(posterior_median_diff,
     main   = "Posterior: Median Difference",
     xlab   = "Median difference",
     col    = "darkcyan",
     border = "white",
     breaks = 50,
     freq   = FALSE)
abline(v = 0, col = "red", lwd = 2, lty = 2)

# --- Plot 5: Posterior of SD difference ---
hist(posterior_sd_diff,
     main   = "Posterior: SD Difference",
     xlab   = "SD difference",
     col    = "darkorange",
     border = "white",
     breaks = 50,
     freq   = FALSE)
abline(v = 0, col = "red", lwd = 2, lty = 2)

# --- Plot 6: Posterior of 90th percentile difference ---
hist(posterior_p90_diff,
     main   = "Posterior: 90th Pct Difference",
     xlab   = "90th percentile difference",
     col    = "forestgreen",
     border = "white",
     breaks = 50,
     freq   = FALSE)
abline(v = 0, col = "red", lwd = 2, lty = 2)

# ============================================================
# 7. FINAL SUMMARY
# ============================================================

cat("\n=== Final Summary ===\n")
cat(sprintf("Observed mean difference:     %.2f points\n",
            mean(method_A) - mean(method_B)))
cat(sprintf("Posterior mean difference:    %.2f points\n",
            mean(posterior_diff)))
cat(sprintf("95%% HDI:                     [%.2f, %.2f]\n",
            quantile(posterior_diff, 0.025),
            quantile(posterior_diff, 0.975)))
cat(sprintf("P(A better than B | data):   %.1f%%\n",
            p_A_better * 100))
cat(sprintf("P(diff > 5 points | data):   %.1f%%\n",
            p_large_gap * 100))