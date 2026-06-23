# ============================================================
#  Two-Part Hurdle Model for Intermittent Demand Forecasting
#  Part 1: Logistic regression  → P(demand > 0) this month
#  Part 2: Linear/log regression → How much demand, given > 0
#  Data: Monthly consumption (Jul 2023 – Apr 2026)
# ============================================================

# install.packages(c("ggplot2", "dplyr", "tidyr", "pscl", "MASS", "lubridate"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(pscl)        # hurdle() function
library(MASS)        # glm.nb for negative binomial
library(lubridate)

set.seed(42)

# ============================================================
# STEP 1 – Build feature-rich data frame
# ============================================================
# The hurdle model shines when you feed it predictors (covariates).
# Even without external drivers, we can engineer time features:
#   - month_num   : seasonal position (1–12)
#   - trend       : linear time index
#   - lag1, lag2  : previous demand values (autoregressive signal)
#   - quarter     : Q1–Q4 seasonal grouping
#   - since_last  : months since last non-zero demand (recency)

demand <- c(24,0,0,0,20,0,0,0,24,0,0,0,0,0,0,0,20,0,0,4,0,20,0,20,8,0,0,20,0,28,0,0,20,0)
dates  <- seq(as.Date("2023-07-01"), by = "month", length.out = length(demand))

df <- data.frame(
  fecha     = dates,
  consumo   = demand,
  trend     = seq_along(demand),
  month_num = month(dates),
  quarter   = quarter(dates)
)

# Lag features
df$lag1 <- c(NA, head(demand, -1))
df$lag2 <- c(NA, NA, head(demand, -2))

# Months since last non-zero demand
since_last <- rep(NA, length(demand))
last_event <- NA
for (i in seq_along(demand)) {
  if (!is.na(last_event)) since_last[i] <- i - last_event
  if (demand[i] > 0) last_event <- i
}
df$since_last <- since_last

# Binary indicator: was there demand? (response for Part 1)
df$demand_occurred <- as.integer(df$consumo > 0)

# Remove rows with NAs from lags
df_model <- df %>% filter(!is.na(lag2))

cat("=== DATA WITH FEATURES (first 8 rows) ===\n")
print(head(df_model, 8))
cat("\nTotal rows for modelling:", nrow(df_model), "\n\n")


# ============================================================
# STEP 2 – Part 1: Logistic regression (will demand occur?)
# ============================================================
# Response  : demand_occurred (0 or 1)
# Predictors: trend, month seasonality, recency, lags

logit_fit <- glm(
  demand_occurred ~ trend + factor(month_num) + since_last + lag1,
  data   = df_model,
  family = binomial(link = "logit")
)

cat("=== PART 1: LOGISTIC REGRESSION (P(demand > 0)) ===\n")
print(summary(logit_fit))

# In-sample predicted probabilities
df_model$prob_demand <- predict(logit_fit, type = "response")


# ============================================================
# STEP 3 – Part 2: Size model (how much demand, given > 0?)
# ============================================================
# Only fit on rows where demand actually occurred
df_nonzero <- df_model %>% filter(consumo > 0)

cat("\n=== NON-ZERO DEMAND OBSERVATIONS ===\n")
print(df_nonzero %>% select(fecha, consumo, trend, month_num, since_last, lag1))

# Try log-normal OLS on non-zero demand
size_fit_lm <- lm(
  log(consumo) ~ trend + since_last,
  data = df_nonzero
)

cat("\n=== PART 2: SIZE MODEL (log-linear OLS on non-zero demand) ===\n")
print(summary(size_fit_lm))

# Also fit a Gamma GLM as alternative
size_fit_gamma <- glm(
  consumo ~ trend + since_last,
  data   = df_nonzero,
  family = Gamma(link = "log")
)

cat("\n=== PART 2 (alt): GAMMA GLM ===\n")
print(summary(size_fit_gamma))


# ============================================================
# STEP 4 – Combined hurdle() model (pscl package)
# ============================================================
# hurdle() fits both parts jointly:
#   count part    : negative binomial (handles size)
#   zero part     : binomial logit    (handles occurrence)
# Note: requires count data (integers), so consumo must be integer

df_model$consumo_int <- as.integer(df_model$consumo)

hurdle_fit <- hurdle(
  consumo_int ~ trend + factor(month_num) + since_last + lag1 |
    trend + since_last + lag1,
  data = df_model,
  dist = "negbin",         # negative binomial for sizes
  zero.dist = "binomial"   # logistic for occurrence
)

cat("\n=== HURDLE MODEL (pscl::hurdle) ===\n")
print(summary(hurdle_fit))


# ============================================================
# STEP 5 – Forecast next 6 months
# ============================================================
h            <- 6
future_dates <- seq(as.Date("2026-05-01"), by = "month", length.out = h)

# Build future feature frame
# We use last known values for lag1, lag2, since_last
last_row      <- tail(df_model, 1)
last_demand   <- tail(demand, 1)
last_nonzero  <- max(which(demand > 0))

future_df <- data.frame(
  fecha     = future_dates,
  trend     = max(df_model$trend) + seq_len(h),
  month_num = month(future_dates),
  quarter   = quarter(future_dates),
  lag1      = c(last_demand, rep(0, h - 1)),   # conservative: assume 0 going forward
  since_last = max(df_model$since_last, na.rm = TRUE) + seq_len(h)
)

# Part 1: P(demand > 0)
future_df$prob_demand <- predict(logit_fit,
                                 newdata = future_df,
                                 type    = "response")

# Part 2: Expected size given demand > 0
future_df$expected_size_lm <- exp(
  predict(size_fit_lm, newdata = future_df) + 
    summary(size_fit_lm)$sigma^2 / 2  # bias correction for log-normal
)

future_df$expected_size_gamma <- predict(size_fit_gamma,
                                         newdata = future_df,
                                         type    = "response")

# Combined expected demand = P(occur) × E(size | occur)
future_df$forecast_lognormal <- future_df$prob_demand * future_df$expected_size_lm
future_df$forecast_gamma      <- future_df$prob_demand * future_df$expected_size_gamma

cat("\n=== FORECAST: NEXT 6 MONTHS ===\n")
future_df %>%
  dplyr::select(fecha, prob_demand, expected_size_lm, forecast_lognormal, forecast_gamma) %>%
  mutate(across(where(is.numeric), ~round(., 2))) 


# ============================================================
# STEP 6 – Simulate prediction intervals via Monte Carlo
# ============================================================
n_sim <- 5000
sim_matrix <- matrix(0, nrow = n_sim, ncol = h)

sigma_lm <- summary(size_fit_lm)$sigma  # residual SD of log(demand)

for (s in seq_len(n_sim)) {
  for (t in seq_len(h)) {
    # Draw occurrence from Bernoulli
    occurs <- rbinom(1, 1, future_df$prob_demand[t])
    if (occurs == 1) {
      # Draw size from log-normal
      log_mu <- predict(size_fit_lm, newdata = future_df[t, ])
      size   <- exp(rnorm(1, mean = log_mu, sd = sigma_lm))
      sim_matrix[s, t] <- max(0, size)
    }
  }
}

pred_intervals <- data.frame(
  fecha        = future_dates,
  mean         = round(colMeans(sim_matrix), 1),
  median       = round(apply(sim_matrix, 2, median), 1),
  p10          = round(apply(sim_matrix, 2, quantile, 0.10), 1),
  p25          = round(apply(sim_matrix, 2, quantile, 0.25), 1),
  p75          = round(apply(sim_matrix, 2, quantile, 0.75), 1),
  p90          = round(apply(sim_matrix, 2, quantile, 0.90), 1),
  prob_nonzero = round(apply(sim_matrix, 2, function(x) mean(x > 0)) * 100, 1)
)

cat("\n=== MONTE CARLO PREDICTION INTERVALS ===\n")
print(pred_intervals)


# ============================================================
# STEP 7 – Model diagnostics
# ============================================================
# In-sample fitted values from hurdle model
df_model$fitted_hurdle <- predict(hurdle_fit, type = "response")

# Pearson residuals
df_model$resid_hurdle <- df_model$consumo_int - df_model$fitted_hurdle

cat("\n=== IN-SAMPLE FIT METRICS (Hurdle model) ===\n")
cat("MAE :", round(mean(abs(df_model$resid_hurdle)), 3), "\n")
cat("RMSE:", round(sqrt(mean(df_model$resid_hurdle^2)), 3), "\n")
cat("Bias:", round(mean(df_model$resid_hurdle), 3), "\n")

# Count zeros predicted vs actual
actual_zeros    <- sum(df_model$consumo_int == 0)
predicted_zeros <- sum(predict(hurdle_fit, type = "prob")[, 1] > 0.5)
cat("Actual zeros   :", actual_zeros, "\n")
cat("Predicted zeros:", predicted_zeros, "\n\n")


# ============================================================
# STEP 8 – Visualisations
# ============================================================
hist_df <- data.frame(fecha = dates, consumo = demand)

# --- Plot 1: In-sample fit ----------------------------------
p1 <- ggplot() +
  geom_col(data = hist_df, aes(x = fecha, y = consumo),
           fill = "#90CAF9", alpha = 0.8, width = 25) +
  geom_line(data = df_model, aes(x = fecha, y = fitted_hurdle),
            color = "#E53935", linewidth = 1.2) +
  geom_point(data = df_model, aes(x = fecha, y = fitted_hurdle),
             color = "#E53935", size = 2.5) +
  labs(
    title    = "Hurdle Model – In-Sample Fit",
    subtitle = "Bars = actual demand | Red line = hurdle model fitted values",
    x = "Date", y = "Consumption"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p1)


# --- Plot 2: Forecast fan chart -----------------------------
p2 <- ggplot() +
  geom_col(data = hist_df, aes(x = fecha, y = consumo),
           fill = "#90CAF9", alpha = 0.8, width = 25) +
  geom_ribbon(data = pred_intervals,
              aes(x = fecha, ymin = p10, ymax = p90),
              fill = "#7B1FA2", alpha = 0.15) +
  geom_ribbon(data = pred_intervals,
              aes(x = fecha, ymin = p25, ymax = p75),
              fill = "#7B1FA2", alpha = 0.25) +
  geom_line(data = pred_intervals, aes(x = fecha, y = mean),
            color = "#E53935", linewidth = 1.3, linetype = "dashed") +
  geom_point(data = pred_intervals, aes(x = fecha, y = mean),
             color = "#E53935", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2026-04-15")),
             linetype = "dotted", color = "grey50") +
  annotate("text", x = as.Date("2026-04-20"), y = max(demand) * 0.95,
           label = "Forecast →", hjust = 0, color = "grey50", size = 3.5) +
  labs(
    title    = "Hurdle Model – Probabilistic Forecast (6 months)",
    subtitle = "Bands: 50% and 80% prediction intervals | Red: mean forecast",
    x = "Date", y = "Consumption"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p2)


# --- Plot 3: Predicted P(demand > 0) over time --------------
prob_df <- bind_rows(
  df_model %>% dplyr::select(fecha, prob_demand) %>% mutate(period = "Historical"),
  future_df %>% dplyr::select(fecha, prob_demand) %>% mutate(period = "Forecast")
)

p3 <- ggplot(prob_df, aes(x = fecha, y = prob_demand * 100, color = period)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("Historical" = "#1976D2", "Forecast" = "#E53935")) +
  labs(
    title    = "Part 1: Predicted Probability of Non-Zero Demand",
    subtitle = "From the logistic regression component",
    x = "Date", y = "P(demand > 0) %", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p3)


# --- Plot 4: Residuals diagnostics --------------------------
p4 <- ggplot(df_model, aes(x = fecha, y = resid_hurdle)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
  geom_col(aes(fill = resid_hurdle > 0), alpha = 0.8, width = 25) +
  scale_fill_manual(values = c("TRUE" = "#43A047", "FALSE" = "#E53935"),
                    labels = c("Under-forecast", "Over-forecast")) +
  labs(
    title    = "Hurdle Model – Residuals",
    subtitle = "Green = model under-predicted | Red = model over-predicted",
    x = "Date", y = "Actual – Fitted", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p4)

cat("=== DONE ===\n")
