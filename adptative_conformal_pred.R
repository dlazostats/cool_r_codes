library(disttree)
library(gamlss.dist)

# ---------------------------------------------------------
# 1. Simulate Heteroscedastic Data
# (Variance explicitly increases as X increases)
# ---------------------------------------------------------
set.seed(123)
n_obs <- 1000
X <- runif(n_obs, 1, 10)
# True model: Y = 3*X + noise, where noise scales with X
Y <- 3 * X + rnorm(n_obs, mean = 0, sd = 0.8 * X) 
df <- data.frame(X = X, Y = Y)

# ---------------------------------------------------------
# 2. Split Data (Train, Calibrate, Test)
# ---------------------------------------------------------
idx_train <- 1:500
idx_calib <- 501:800
idx_test  <- 801:1000

train_data <- df[idx_train, ]
calib_data <- df[idx_calib, ]
test_data  <- df[idx_test, ]

# ---------------------------------------------------------
# 3. Train the Base Estimator (distforest)
# ---------------------------------------------------------
# We assume a Normal distribution family (NO) for the base heuristic
cat("Training distributional forest...\n")
forest_model <- distforest(Y ~ X, data = train_data, family = NO)

# ---------------------------------------------------------
# 4. Calibration Step (Conformal Prediction)
# ---------------------------------------------------------
# Predict mu and sigma for the calibration set
calib_preds <- predict(forest_model, newdata = calib_data, type = "parameter")
mu_calib <- calib_preds$mu
sigma_calib <- calib_preds$sigma

# Calculate the locally scaled non-conformity scores
scores <- abs(calib_data$Y - mu_calib) / sigma_calib

# Set alpha for 90% marginal coverage
alpha <- 0.10
n_calib <- nrow(calib_data)

# Calculate the finite-sample corrected quantile (q_hat)
# Formula: ceiling((n + 1) * (1 - alpha)) / n
quantile_level <- ceiling((n_calib + 1) * (1 - alpha)) / n_calib
q_hat <- quantile(scores, probs = quantile_level)

cat("Calibration complete. Conformal multiplier (q_hat):", round(q_hat, 3), "\n")

# ---------------------------------------------------------
# 5. Prediction Step (Fast and Adaptive Intervals)
# ---------------------------------------------------------
# Predict the parameters for the new test data
test_preds <- predict(forest_model, newdata = test_data, type = "parameter")
mu_test <- test_preds$mu
sigma_test <- test_preds$sigma

# Construct the final, guaranteed prediction intervals
lower_bound <- mu_test - (q_hat * sigma_test)
upper_bound <- mu_test + (q_hat * sigma_test)

# Combine into a final results dataframe
results <- data.frame(
  X = test_data$X,
  True_Y = test_data$Y,
  Pred_Mu = mu_test,
  Lower_PI = lower_bound,
  Upper_PI = upper_bound
)

# Calculate empirical coverage on the test set
covered <- (results$True_Y >= results$Lower_PI) & (results$True_Y <= results$Upper_PI)
empirical_coverage <- mean(covered)

cat("Target Coverage: ", (1 - alpha) * 100, "%\n", sep="")
cat("Empirical Coverage on Test Set: ", empirical_coverage * 100, "%\n", sep="")


# 1. Plot the raw data points
plot(results$X, results$True_Y, 
     main = "Adaptive Conformal Prediction Intervals",
     xlab = "X", ylab = "Y",
     col = "darkgray", pch = 16)

# 2. Get the ordering index based on X
idn <- order(results$X)

# 3. Sort X for the lines
xor <- results$X[idn]

# 4. Draw the lines, applying the same 'idn' sorting index to the Y vectors
lines(xor, results$Lower_PI[idn], col = "blue", lwd = 2, lty = 2)
lines(xor, results$Upper_PI[idn], col = "blue", lwd = 2, lty = 2)

# Optional: Add a legend for clarity
legend("topleft", 
       legend = c("True Y", "90% Prediction Interval"), 
       col = c("darkgray", "blue"), 
       pch = c(16, NA), 
       lty = c(NA, 2), 
       lwd = c(NA, 2))
