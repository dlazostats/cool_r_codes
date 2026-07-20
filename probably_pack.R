# =============================================================================
# Comparing quantregForest prediction intervals with conformal bands (probably)
#   ...now on SIMULATED HETEROSCEDASTIC data with LOCAL structure.
#
# The point of switching away from a real dataset: with heteroscedastic noise we
# KNOW the ground truth, and we can watch constant-width split conformal fail to
# adapt while CQR widens/narrows its band locally.
# =============================================================================
# install.packages(c("tidymodels", "probably", "quantregForest", "ranger"))

library(tidymodels)
library(probably)
library(quantregForest)

# -----------------------------------------------------------------------------
# 1. Simulate heteroscedastic data with local behaviour
#       y = f_mean(x) + N(0, f_sd(x)^2)
#
#   f_mean(x) = sin(2x) + 0.5 x                    <- wiggly LOCAL mean + drift
#   f_sd(x)   = 0.2 + 0.5*(x/10) + 0.4*sin(x)^2    <- noise is NOT constant:
#                 \_/   \______/    \_________/
#                floor   grows w/ x   local pockets of high/low variance
#
#   Half-width of the true 95% band therefore swings ~5x across the domain,
#   which is exactly the regime where an adaptive band should pay off.
# -----------------------------------------------------------------------------
f_mean <- function(x) sin(2 * x) + 0.5 * x
f_sd   <- function(x) 0.2 + 0.5 * (x / 10) + 0.4 * sin(x)^2

sim_data <- function(n) {
  x <- runif(n, 0, 10)
  y <- f_mean(x) + rnorm(n, mean = 0, sd = f_sd(x))
  data.frame(x = x, y = y)
}

form <- y ~ x

# -----------------------------------------------------------------------------
#    Data + split  (simulate a training pool and an independent test set;
#    the test set is identical for every method)
# -----------------------------------------------------------------------------
set.seed(42)
train_full <- sim_data(1500)   # training pool
test_data  <- sim_data(500)    # independent test set  <-- same for all methods
plot(train_full$y,train_full$x)

# Conformal methods need a calibration set NOT used to fit the model.
# Carve it out of the training pool (proper-train / calibration = 75 / 25).
set.seed(123)
cal_idx      <- sample(seq_len(nrow(train_full)), 0.25 * nrow(train_full))
cal_data     <- train_full[cal_idx, ]
proper_train <- train_full[-cal_idx, ]

target_cov <- 0.95

# -----------------------------------------------------------------------------
# 2. Fit a workflow on the proper-training set
#    (needed as the `object` argument for the conformal functions)
# -----------------------------------------------------------------------------
rf_spec <- rand_forest(trees = 1000) |>
  set_engine("ranger") |>
  set_mode("regression")

rf_wflow <- workflow() |>
  add_model(rf_spec) |>
  add_formula(form)

rf_fit <- fit(rf_wflow, data = proper_train)

# -----------------------------------------------------------------------------
# 3a. SPLIT CONFORMAL  -> constant-width bands, guaranteed marginal coverage
#     level is set in predict(). On heteroscedastic data this band is the SAME
#     width everywhere: too wide in quiet regions, too narrow in noisy ones.
# -----------------------------------------------------------------------------
split_obj  <- int_conformal_split(rf_fit, cal_data)
split_pred <- predict(split_obj, test_data, level = target_cov)

# -----------------------------------------------------------------------------
# 3b. CONFORMALIZED QUANTILE REGRESSION (CQR)  -> adaptive + guaranteed
#     Fits a quantregForest INTERNALLY (ntree passed via ...), then calibrates.
#     This is the direct analogue of your quantregForest intervals, and it is
#     what SHOULD track the local noise level.  level is set HERE, not in predict().
# -----------------------------------------------------------------------------
cqr_obj  <- int_conformal_quantile(rf_fit, proper_train, cal_data,
                                   level = target_cov, ntree = 1000)
cqr_pred <- predict(cqr_obj, test_data)

# -----------------------------------------------------------------------------
# 3c. PLAIN quantregForest (no conformal), retrained on the SAME proper_train,
#     so the ONLY difference vs CQR is the conformal calibration step.
# -----------------------------------------------------------------------------
Xtr <- proper_train[, "x", drop = FALSE]
Ytr <- proper_train$y
Xte <- test_data[, "x", drop = FALSE]

qrf_plain <- quantregForest(x = Xtr, y = Ytr, ntree = 1000)
q_pred    <- predict(qrf_plain, newdata = Xte,
                     what = c((1 - target_cov) / 2, 1 - (1 - target_cov) / 2))

# -----------------------------------------------------------------------------
# 4. Evaluate: empirical coverage + mean interval width on the SAME test set
# -----------------------------------------------------------------------------
eval_int <- function(lower, upper, truth, name) {
  data.frame(
    Method     = name,
    Coverage   = round(100 * mean(truth >= lower & truth <= upper), 1),
    Mean_Width = round(mean(upper - lower), 1)
  )
}

comparison <- rbind(
  eval_int(q_pred[, 1],            q_pred[, 2],            test_data$y,
           "quantregForest (raw)"),
  eval_int(split_pred$.pred_lower, split_pred$.pred_upper, test_data$y,
           "Split conformal (const width)"),
  eval_int(cqr_pred$.pred_lower,   cqr_pred$.pred_upper,   test_data$y,
           "CQR (QRF + conformal)")
)

cat("\nTarget coverage:", target_cov * 100, "%   (n_test =",
    nrow(test_data), ")\n\n")
print(comparison, row.names = FALSE)

# A sharper diagnostic than marginal coverage: does coverage hold LOCALLY?
# Split the test set into a low-noise half and a high-noise half of x-space.
noisy <- f_sd(test_data$x) > median(f_sd(test_data$x))
local_cov <- function(lower, upper) {
  hit <- test_data$y >= lower & test_data$y <= upper
  c(quiet = round(100 * mean(hit[!noisy]), 1),
    noisy = round(100 * mean(hit[ noisy]), 1))
}
cat("\nLocal coverage (quiet vs noisy region):\n")
cat("  Split conformal:",
    paste(local_cov(split_pred$.pred_lower, split_pred$.pred_upper),
          collapse = " / "), "\n")
cat("  CQR            :",
    paste(local_cov(cqr_pred$.pred_lower, cqr_pred$.pred_upper),
          collapse = " / "), "\n")

# -----------------------------------------------------------------------------
# 5. Visual comparison  (sorted by x -- the natural axis for 1-D heteroscedasticity)
#    Oracle band = f_mean(x) +/- 1.96 * f_sd(x) is the TRUE 95% interval.
# -----------------------------------------------------------------------------
ord <- order(test_data$x)
xo  <- test_data$x[ord]

plot(test_data$x, test_data$y, pch = 19, col = "grey65", cex = 0.5,
     ylab = "y", xlab = "x",
     main = "95% bands on heteroscedastic data: split conformal vs CQR")

# oracle (true) 95% band
lines(xo, f_mean(xo) + qnorm(0.975) * f_sd(xo), col = "darkgreen", lwd = 2)
lines(xo, f_mean(xo) - qnorm(0.975) * f_sd(xo), col = "darkgreen", lwd = 2)

lines(xo, split_pred$.pred_lower[ord], col = "blue", lty = 2)
lines(xo, split_pred$.pred_upper[ord], col = "blue", lty = 2)
lines(xo, cqr_pred$.pred_lower[ord],   col = "red")
lines(xo, cqr_pred$.pred_upper[ord],   col = "red")

legend("topleft",
       c("Data", "Oracle band", "Split conformal", "CQR"),
       pch = c(19, NA, NA, NA), lty = c(NA, 1, 2, 1), lwd = c(NA, 2, 1, 1),
       col = c("grey65", "darkgreen", "blue", "red"), bty = "n")
