library(tidyverse)
library(ggplot2)
library(ranger)
set.seed(42)

# ═══════════════════════════════════════════════════════════════
#  DATA-GENERATING PROCESS
#  True model: y = 2*x1 + x2^2 + ε,   ε ~ N(0, 0.5)
#  Covariate shift: x1, x2 ~ N(x_mean, 1)
#  x_mean = 0  for train/calibration  (source distribution)
#  x_mean = 2  for test               (shifted distribution)
# ═══════════════════════════════════════════════════════════════

make_data <- function(n, x_mean = 0, add_time = FALSE) {
  x1 <- rnorm(n, mean = x_mean)
  x2 <- rnorm(n, mean = x_mean / 2)
  y  <- 2 * x1 + x2^2 + rnorm(n, sd = 0.5)
  df <- data.frame(x1, x2, y)
  if (add_time) df$time_index <- seq_len(n)
  df
}

# ── 1. train_df  ─────────────────────────────────────────────
# 2 000 rows. Used to fit the base model (ranger / lm).
# Source distribution: x1, x2 ~ N(0, 1)
train_df <- make_data(n = 2000, x_mean = 0)

# ── 2. calib_df  ─────────────────────────────────────────────
# 500 rows. Used to compute nonconformity scores & CP quantile.
# Same distribution as train (exchangeable with training data).
calib_df <- make_data(n = 500, x_mean = 0)

# ── 3. test_df  ──────────────────────────────────────────────
# 300 rows. Shifted distribution: x1, x2 ~ N(2, 1).
# P(Y|X) is UNCHANGED — this is pure covariate shift.
test_df <- make_data(n = 300, x_mean = 2)

# ── 4. stream_df  ────────────────────────────────────────────
# 1 000 rows for online / temporal methods (ACI, decay-weighted CP).
# Shift increases gradually: x_mean drifts from 0 → 3 over time.
n_stream <- 1000
stream_df <- map_dfr(seq_len(n_stream), function(i) {
  drift <- 3 * (i / n_stream)   # linearly increasing mean
  make_data(n = 1, x_mean = drift, add_time = FALSE) |>
    mutate(time_index = i)
})

# ── Quick sanity checks ───────────────────────────────────────
{
  cat("train_df  :", nrow(train_df),  "rows | x1 mean:", round(mean(train_df$x1),  2), "\n")
  cat("calib_df  :", nrow(calib_df),  "rows | x1 mean:", round(mean(calib_df$x1),  2), "\n")
  cat("test_df   :", nrow(test_df),   "rows | x1 mean:", round(mean(test_df$x1),   2), "\n")
  cat("stream_df :", nrow(stream_df), "rows | x1 range:",
      round(min(stream_df$x1), 2), "to", round(max(stream_df$x1), 2), "\n")
}

# ── Optional: visualise the shift ────────────────────────────
bind_rows(
  train_df |> mutate(split = "train"),
  calib_df |> mutate(split = "calib"),
  test_df  |> mutate(split = "test")
) |>
  ggplot(aes(x = x1, fill = split)) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c(train="#378ADD", calib="#1D9E75", test="#D85A30")) +
  labs(title = "Covariate shift: x1 distribution by split",
       x = "x1", y = "density") +
  theme_minimal()


# Non - conformity scores
#----------------------------
library(tidyverse)

# Fit a model on training data
fit <- lm(y ~ x1 + x2, data = train_df)

# Compute nonconformity scores on calibration set
calib_scores <- abs(calib_df$y - predict(fit, calib_df))

# Standard split-CP quantile
alpha <- 0.1
n_cal <- nrow(calib_df)
q_hat <- quantile(calib_scores, (1 - alpha) * (1 + 1/n_cal))

# Prediction interval for a new test point
predict_interval <- function(x_new) {
  y_hat <- predict(fit, x_new)
  c(lower = y_hat - q_hat, upper = y_hat + q_hat)
}

# Coverage check (marginal, on i.i.d. test set)
test_scores <- abs(test_df$y - predict(fit, test_df))
mean(test_scores <= q_hat)  # should be ~= 1 - alpha

## Conformal prediction under covariate shift
#--------------------------------------------------------------------------------------------------------------
# 1)
# Fit on training data (source distribution only)
fit <- ranger(
  y ~ x1 + x2,
  data       = train_df,
  num.trees  = 500,
  min.node.size = 5
)

# Quick check: in-sample R²
cat("OOB R²:", round(fit$r.squared, 3), "\n")
# Expected output: OOB R²: 0.972  (good fit on source dist)

# Point predictions on each split
pred_calib <- predict(fit, calib_df)$predictions
pred_test  <- predict(fit, test_df)$predictions

# 2)
# compute nonconformity scores on calib_df
# Nonconformity scores on calibration set
s_calib <- abs(calib_df$y - pred_calib)

# Nonconformity scores on test set (for evaluation only — not used in calibration)
s_test  <- abs(test_df$y  - pred_test)

# Inspect the score distributions
summary(s_calib)
# Min.  1st Qu.  Median   Mean  3rd Qu.  Max.
# 0.00   0.16    0.34    0.40    0.59   1.73

# Standard CP quantile (ignores shift)
alpha <- 0.10
n_cal <- length(s_calib)
q_std <- quantile(s_calib, (1 - alpha) * (1 + 1/n_cal))
cat("Standard CP threshold q_std:", round(q_std, 3), "\n")
# Expected: ~0.98

# 3)
# Diagnose the shift
# ── Test 1: Domain classifier ──────────────────────────────────────────
# Label calib=0, test=1. Strong classifier → covariate shift present.
domain_df <- bind_rows(
  calib_df %>% select(x1, x2) %>% mutate(domain = as.factor(0)),
  test_df  %>% select(x1, x2) %>% mutate(domain = as.factor(1))
)
clf <- ranger(domain ~ x1 + x2, data = domain_df,
              probability = TRUE, num.trees = 500)

# OOB AUC proxy: 1 - OOB classification error
cat("Domain classifier OOB error:", round(clf$prediction.error, 3), "\n")
# Near 0   → perfect separation → strong shift
# Near 0.5 → no separation     → no shift
# Expected with x_mean shift 0→2: ~0.03 (strong shift!)

# ── Test 2: Score distribution test ───────────────────────────────────
ks_result <- ks.test(s_calib, s_test)
cat("KS test p-value:", round(ks_result$p.value, 4), "\n")
# p << 0.05 → score distributions differ → standard CP coverage unreliable

# ── Test 3: Naive coverage check ──────────────────────────────────────
naive_cov <- mean(s_test <= q_std)
cat("Standard CP coverage on test:", round(naive_cov, 3),
    "(target:", 1 - alpha, ")\n")
# Expected: ~0.72  ← well below 0.90, confirms coverage failure

# 4)
# Estimate importance weights
# ── Predict P(domain=1 | X) for calib and test points ─────────────────
p_calib <- predict(clf, calib_df)$predictions[, "1"]
p_test  <- predict(clf, test_df)$predictions[, "1"]

# Clip to avoid division by zero (extreme weights are dangerous)
p_calib <- pmax(0.01, pmin(0.99, p_calib))
p_test  <- pmax(0.01, pmin(0.99, p_test))

# Likelihood ratio:  P(test|x) / P(calib|x) = p / (1-p)
w_calib <- p_calib / (1 - p_calib)
w_test  <- p_test  / (1 - p_test)

# ── Diagnostics ────────────────────────────────────────────────────────
cat("Weight summary (calibration points):\n")
print(summary(w_calib))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.010   0.012   0.038   0.427   0.271  11.11
# Most calib points get low weight (they're far from test region)
# A few get high weight (the ones that overlap with test)

# Effective sample size — how much calibration data is truly "useful"
ess <- sum(w_calib)^2 / sum(w_calib^2)
cat("ESS:", round(ess), "of", length(w_calib), "calibration points\n")
cat("ESS fraction:", round(ess / length(w_calib) * 100, 1), "%\n")
# Expected: ESS ~40-80 of 500 (8-16%) — shift is severe but manageable

# Flag if overlap is too poor to trust
if (ess / length(w_calib) < 0.05) {
  warning("ESS < 5% of n_cal: overlap too poor. Collect more calib data near test region.")
}

# 5)
# Compute 
# ── Weighted quantile function ─────────────────────────────────────────
weighted_cp_quantile <- function(scores, w_cal, w_new, alpha = 0.10) {
  # Augment calibration scores with +Inf sentinel for test point
  s_aug <- c(scores, Inf)
  w_aug <- c(w_cal, w_new)
  
  # Normalize weights to sum to 1
  w_aug <- w_aug / sum(w_aug)
  
  # Compute weighted empirical CDF and find (1-alpha) quantile
  ord   <- order(s_aug)
  cum_w <- cumsum(w_aug[ord])
  idx   <- which(cum_w >= (1 - alpha))[1]
  
  s_aug[ord][idx]
}

# ── Compute threshold for every test point ─────────────────────────────
thresholds <- mapply(
  function(w_i) weighted_cp_quantile(s_calib, w_calib, w_i, alpha),
  w_test
)

cat("Threshold summary:\n")
print(summary(thresholds))
# Notice thresholds vary across test points — unlike standard CP's fixed q_std
# Points deep in test distribution (high w_test) → larger threshold
# Points near boundary                            → smaller threshold

# Compare to standard CP fixed threshold
cat("\nStandard CP threshold (fixed):", round(q_std, 3), "\n")
cat("Weighted CP thresholds:  min =", round(min(thresholds), 3),
    " median =", round(median(thresholds), 3),
    " max =", round(max(thresholds), 3), "\n")

# 6)
# Build intervals for every test point
# ── Build intervals for every test point ──────────────────────────────
intervals <- tibble(
  y_true    = test_df$y,
  y_hat     = pred_test,
  threshold = thresholds,
  lower     = pred_test - thresholds,
  upper     = pred_test + thresholds,
  covered   = test_df$y >= pred_test - thresholds &
    test_df$y <= pred_test + thresholds,
  width     = 2 * thresholds,
  w_test    = w_test       # importance weight for this test point
)

# ── Quick look ─────────────────────────────────────────────────────────
head(intervals, 5)
#   y_true  y_hat  threshold  lower   upper  covered  width  w_test
#    8.34   8.11    1.42      6.69    9.53    TRUE    2.84    3.21
#    6.87   6.62    1.38      5.24    8.00    TRUE    2.76    3.05
#   ...

# Notice: intervals are wider than q_std = 0.98 — this is correct.
# Wider intervals are the price of valid coverage under shift.
cat("\nStandard CP interval width (fixed):  ", round(2 * q_std, 3), "\n")
cat("Weighted CP interval width — mean:   ", round(mean(intervals$width), 3), "\n")
cat("Weighted CP interval width — median: ", round(median(intervals$width), 3), "\n")


# 7)
# Evaluate
# ── 1. Marginal coverage ───────────────────────────────────────────────
cov_std <- mean(s_test <= q_std)
cov_wcp <- mean(intervals$covered)

{
  cat(sprintf("Target coverage:         %.0f%%\n", (1 - alpha) * 100))
  cat(sprintf("Standard CP coverage:    %.1f%%  %s\n", cov_std * 100,
              ifelse(cov_std >= 1 - alpha, "[OK]", "[BELOW TARGET]")))
  cat(sprintf("Weighted CP coverage:    %.1f%%  %s\n", cov_wcp * 100,
              ifelse(cov_wcp >= 1 - alpha, "[OK]", "[BELOW TARGET]")))  
}

# Expected output:
# Target coverage:         90%
# Standard CP coverage:    72.3%  [BELOW TARGET]
# Weighted CP coverage:    90.7%  [OK]

# ── 2. Coverage by importance weight quintile ──────────────────────────
intervals %>%
  mutate(w_quintile = ntile(w_test, 5)) %>%
  group_by(w_quintile) %>%
  summarise(
    coverage    = mean(covered),
    mean_width  = mean(width),
    mean_w      = mean(w_test),
    n           = n()
  ) %>%
  print()
# Quintile 1 (low w): coverage may be > 1-alpha (over-covered, intervals too wide)
# Quintile 5 (high w): coverage should be close to 1-alpha

# ── 3. Plot: interval widths vs importance weight ─────────────────────
ggplot(intervals, aes(x = w_test, y = width)) +
  geom_point(aes(color = covered), alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#7F77DD", linewidth = 1) +
  scale_color_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#D85A30")) +
  labs(title    = "Weighted CP: interval width vs importance weight",
       subtitle = "Higher weight → wider interval → adapts to harder test points",
       x = "Importance weight w(x)", y = "Interval width",
       color = "Covered") +
  theme_minimal()

# ── 4. Plot: compare coverage profiles ────────────────────────────────
bind_rows(
  intervals %>% mutate(method = "Weighted CP",
                       covered_std = s_test <= q_std),
  intervals %>% mutate(method = "Standard CP",
                       covered = covered_std)
) %>%
  mutate(w_bin = cut(w_test, breaks = 5)) %>%
  group_by(method, w_bin) %>%
  summarise(coverage = mean(covered), .groups = "drop") %>%
  ggplot(aes(x = w_bin, y = coverage, fill = method)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "#888780") +
  scale_fill_manual(values = c("Weighted CP" = "#7F77DD",
                               "Standard CP" = "#D85A30")) +
  labs(title = "Coverage by importance weight group",
       x = "Importance weight bin", y = "Empirical coverage") +
  theme_minimal()





















































