# ============================================================
# SMALL SAMPLE BEHAVIOR
# ============================================================

library(gtools)
library(bayesboot)

set.seed(42)

# Very small sample — where differences matter most
small_data <- c(2.1, 3.4, 1.8, 4.2, 2.9)
n <- length(small_data)
R <- 10000

# --- Classical bootstrap ---
classical <- numeric(R)
for(i in 1:R){
  classical[i] <- mean(sample(small_data, n, replace = TRUE))
}

# --- Bayesian bootstrap ---
bb <- bayesboot(small_data, mean, R = R)

par(mfrow = c(1,2))
hist(classical,
     main   = "Classical Bootstrap\n(small sample)",
     xlab   = "Mean",
     col    = "tomato",
     border = "white",
     breaks = 30)

hist(bb$V1,
     main   = "Bayesian Bootstrap\n(small sample)",
     xlab   = "Mean",
     col    = "steelblue",
     border = "white",
     breaks = 30)

cat("Classical — unique values possible:", length(unique(classical)), "\n")
cat("Bayesian  — unique values possible:", length(unique(bb$V1)), "\n")
# Classical produces far fewer unique values (discrete problem)

# Useful decision guide:

decision_guide <- data.frame(
  Situation = c(
    "Need to publish in traditional journal",
    "Small sample (n < 20)",
    "Need P(parameter > X) statement",
    "Want to incorporate prior knowledge",
    "Comparing medians or percentiles",
    "Large sample (n > 100)",
    "Explaining results to non-statisticians",
    "Exploratory / internal analysis",
    "Regulatory or clinical context",
    "Outliers present in data"
  ),
  Classical = c("✅", "⚠️", "❌", "❌", "✅", "✅", "✅", "✅", "✅", "⚠️"),
  Bayesian_Bootstrap = c("⚠️", "✅", "✅", "❌", "✅", "✅", "⚠️", "✅", "⚠️", "⚠️"),
  Full_Bayesian = c("⚠️", "✅", "✅", "✅", "✅", "✅", "❌", "⚠️", "⚠️", "✅")
)

print(decision_guide)

# outlier problem
# Bayesian bootstrap is MORE sensitive to outliers
# because every observation always gets a weight > 0

set.seed(42)
data_clean   <- c(10, 12, 11, 13, 10, 12, 11)
data_outlier <- c(10, 12, 11, 13, 10, 12, 50)  # one outlier

bb_clean   <- bayesboot(data_clean,   mean, R = 10000)
bb_outlier <- bayesboot(data_outlier, mean, R = 10000)

cl_clean   <- replicate(10000, mean(sample(data_clean,   replace = TRUE)))
cl_outlier <- replicate(10000, mean(sample(data_outlier, replace = TRUE)))

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(cl_clean,   main = "Classical — Clean",   col = "tomato",    border = "white", breaks = 30)
hist(cl_outlier, main = "Classical — Outlier",  col = "tomato",    border = "white", breaks = 30)
hist(bb_clean$V1,   main = "Bayesian — Clean",  col = "steelblue", border = "white", breaks = 30)
hist(bb_outlier$V1, main = "Bayesian — Outlier", col = "steelblue", border = "white", breaks = 30)

cat("\n=== Outlier impact on posterior mean ===\n")
cat(sprintf("Classical clean:    %.2f\n", mean(cl_clean)))
cat(sprintf("Classical outlier:  %.2f\n", mean(cl_outlier)))
cat(sprintf("Bayesian clean:     %.2f\n", mean(bb_clean$V1)))
cat(sprintf("Bayesian outlier:   %.2f\n", mean(bb_outlier$V1)))
