# ============================================================
#  Zero-Inflated Poisson & Negative Binomial
#  for Intermittent Demand Forecasting
#
#  Key difference from Hurdle:
#  - Hurdle  : zeros come ONLY from the binary part (structural zeros)
#  - ZIP/ZINB: zeros can come from BOTH the binary part (structural)
#              AND the count part (sampling zeros) — more flexible
#
#  Data: Monthly consumption (Jul 2023 – Apr 2026)
# ============================================================

# install.packages(c("pscl", "MASS", "ggplot2", "dplyr", "tidyr",
#                    "lubridate", "lmtest", "AER"))
library(pscl)        # zeroinfl()
library(MASS)        # glm.nb(), negative binomial
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(lmtest)      # lrtest(), coeftest()
library(AER)        # dispersiontest()

set.seed(42)

# ============================================================
# STEP 1 – Data & feature engineering
# ============================================================
demand <- c(24,0,0,0,20,0,0,0,24,0,0,0,0,0,0,0,20,0,0,4,0,20,0,20,8,0,0,20,0,28,0,0,20,0)
dates  <- seq(as.Date("2023-07-01"), by = "month", length.out = length(demand))

df <- data.frame(
  fecha      = dates,
  consumo    = as.integer(demand),
  trend      = seq_along(demand),
  month_num  = month(dates),
  quarter    = quarter(dates),
  sem1       = as.integer(month(dates) <= 6),   # first half of year flag
  lag1       = c(NA, head(as.integer(demand), -1)),
  lag2       = c(NA, NA, head(as.integer(demand), -2))
)

# Months since last non-zero demand
since_last <- rep(NA_integer_, nrow(df))
last_event <- NA_integer_
for (i in seq_len(nrow(df))) {
  if (!is.na(last_event)) since_last[i] <- i - last_event
  if (df$consumo[i] > 0)  last_event    <- i
}
df$since_last <- since_last

# Binary: demand occurred?
df$occurred <- as.integer(df$consumo > 0)

# Drop NA rows from lags
df_model <- df %>% filter(!is.na(lag2))

cat("=== DATA SUMMARY ===\n")
cat("N observations   :", nrow(df_model), "\n")
cat("Zero periods     :", sum(df_model$consumo == 0), "\n")
cat("Non-zero periods :", sum(df_model$consumo  > 0), "\n")
cat("% zeros          :", round(mean(df_model$consumo == 0) * 100, 1), "%\n")
cat("Mean demand      :", round(mean(df_model$consumo), 2), "\n")
cat("Variance         :", round(var(df_model$consumo), 2),
    " (>> mean → overdispersion → NegBin preferred)\n\n")


# ============================================================
# STEP 2 – Overdispersion test
# ============================================================
# Poisson assumes mean == variance.
# If variance >> mean, data is overdispersed → use NegBin.

poisson_base <- glm(consumo ~ trend, data = df_model, family = poisson)
disp_test    <- dispersiontest(poisson_base, trafo = 1)

cat("=== OVERDISPERSION TEST (Poisson baseline) ===\n")
print(disp_test)
cat("Dispersion ratio:", round(disp_test$estimate, 3),
    ifelse(disp_test$estimate > 1,
           " → Overdispersed: NegBin recommended\n\n",
           " → No overdispersion: Poisson may suffice\n\n"))


# ============================================================
# STEP 3 – Fit four competing models
# ============================================================
# Formula for count part  : trend + seasonality + recency + lag
# Formula for zero part   : since_last + lag1 (drivers of excess zeros)

# Pre-create factor columns — avoids formula parsing errors inside update()
# Use quarter (4 levels) instead of month (12 levels): with only ~10 non-zero
# events, estimating 11 month dummies causes optim() to diverge.
df_model$quarter_f <- factor(df_model$quarter)

count_formula <- consumo ~ trend + quarter_f + since_last + lag1
zero_formula  <-         ~ since_last + lag1

# --- Model A: Standard Poisson (baseline) -------------------
fit_poisson <- glm(count_formula, data = df_model, family = poisson)

# --- Model B: Negative Binomial (overdispersion) ------------
fit_nb <- glm.nb(count_formula, data = df_model)

# --- Model C: Zero-Inflated Poisson (ZIP) -------------------
fit_zip <- zeroinfl(
  consumo ~ trend + quarter_f + since_last + lag1 | since_last + lag1,
  data = df_model,
  dist = "poisson"
)

# --- Model D: Zero-Inflated Negative Binomial (ZINB) --------
fit_zinb <- zeroinfl(
  consumo ~ trend + quarter_f + since_last + lag1 | since_last + lag1,
  data = df_model,
  dist = "negbin"
)

cat("=== MODEL SUMMARIES ===\n\n")
cat("--- A: Poisson ---\n");     print(summary(fit_poisson))
cat("--- B: Negative Binomial ---\n"); print(summary(fit_nb))
cat("--- C: ZIP ---\n");         print(summary(fit_zip))
cat("--- D: ZINB ---\n");        print(summary(fit_zinb))


# ============================================================
# STEP 4 – Model comparison (AIC / BIC / log-likelihood)
# ============================================================
model_comparison <- data.frame(
  Model   = c("Poisson", "NegBin", "ZIP", "ZINB"),
  AIC     = round(c(AIC(fit_poisson), AIC(fit_nb), AIC(fit_zip), AIC(fit_zinb)), 2),
  BIC     = round(c(BIC(fit_poisson), BIC(fit_nb), BIC(fit_zip), BIC(fit_zinb)), 2),
  LogLik  = round(c(logLik(fit_poisson), logLik(fit_nb),
                    logLik(fit_zip),     logLik(fit_zinb)), 2)
)

cat("\n=== MODEL COMPARISON (lower AIC/BIC = better fit) ===\n")
print(model_comparison[order(model_comparison$AIC), ])

best_model_name <- model_comparison$Model[which.min(model_comparison$AIC)]
cat("\nBest model by AIC:", best_model_name, "\n\n")


# ============================================================
# STEP 5 – Vuong test: ZIP vs Poisson & ZINB vs NegBin
# ============================================================
cat("=== VUONG TEST: ZIP vs Poisson ===\n")
print(vuong(fit_zip, fit_poisson))   # positive Z favours ZIP

cat("\n=== VUONG TEST: ZINB vs NegBin ===\n")
print(vuong(fit_zinb, fit_nb))       # positive Z favours ZINB


# ============================================================
# STEP 6 – In-sample fitted values & zero predictions
# ============================================================
df_model$fit_poisson <- predict(fit_poisson, type = "response")
df_model$fit_nb      <- predict(fit_nb,      type = "response")
df_model$fit_zip     <- predict(fit_zip,     type = "response")
df_model$fit_zinb    <- predict(fit_zinb,    type = "response")

# Predicted P(zero) from zero-inflated models
df_model$pzero_zip  <- predict(fit_zip,  type = "zero")
df_model$pzero_zinb <- predict(fit_zinb, type = "zero")

mae  <- function(pred, obs) round(mean(abs(obs - pred)), 3)
rmse <- function(pred, obs) round(sqrt(mean((obs - pred)^2)), 3)
bias <- function(pred, obs) round(mean(pred - obs), 3)

cat("\n=== IN-SAMPLE FIT METRICS ===\n")
fit_metrics <- data.frame(
  Model = c("Poisson", "NegBin", "ZIP", "ZINB"),
  MAE   = c(mae(df_model$fit_poisson, df_model$consumo),
            mae(df_model$fit_nb,      df_model$consumo),
            mae(df_model$fit_zip,     df_model$consumo),
            mae(df_model$fit_zinb,    df_model$consumo)),
  RMSE  = c(rmse(df_model$fit_poisson, df_model$consumo),
            rmse(df_model$fit_nb,      df_model$consumo),
            rmse(df_model$fit_zip,     df_model$consumo),
            rmse(df_model$fit_zinb,    df_model$consumo)),
  Bias  = c(bias(df_model$fit_poisson, df_model$consumo),
            bias(df_model$fit_nb,      df_model$consumo),
            bias(df_model$fit_zip,     df_model$consumo),
            bias(df_model$fit_zinb,    df_model$consumo))
)
print(fit_metrics[order(fit_metrics$MAE), ])


# ============================================================
# STEP 7 – Forecast next 6 months
# ============================================================
h            <- 6
future_dates <- seq(as.Date("2026-05-01"), by = "month", length.out = h)

future_df <- data.frame(
  fecha      = future_dates,
  consumo    = NA_integer_,
  trend      = max(df_model$trend) + seq_len(h),
  month_num  = month(future_dates),
  quarter    = quarter(future_dates),
  sem1       = as.integer(month(future_dates) <= 6),
  lag1       = c(tail(df_model$consumo, 1), rep(0L, h - 1)),
  lag2       = c(tail(df_model$consumo, 2)[1], tail(df_model$consumo, 1), rep(0L, h - 2)),
  since_last = max(df_model$since_last, na.rm = TRUE) + seq_len(h)
)

# Must match the factor levels used during training
future_df$quarter_f <- factor(future_df$quarter, levels = levels(df_model$quarter_f))

future_df$fc_poisson <- predict(fit_poisson, newdata = future_df, type = "response")
future_df$fc_nb      <- predict(fit_nb,      newdata = future_df, type = "response")
future_df$fc_zip     <- predict(fit_zip,     newdata = future_df, type = "response")
future_df$fc_zinb    <- predict(fit_zinb,    newdata = future_df, type = "response")

# P(zero demand) for each future month
future_df$pzero_zip  <- predict(fit_zip,  newdata = future_df, type = "zero")
future_df$pzero_zinb <- predict(fit_zinb, newdata = future_df, type = "zero")
future_df$prob_demand_zip  <- 1 - future_df$pzero_zip
future_df$prob_demand_zinb <- 1 - future_df$pzero_zinb

cat("\n=== FORECAST: NEXT 6 MONTHS ===\n")
future_df %>%
  dplyr::select(fecha, fc_zip, fc_zinb, prob_demand_zip, prob_demand_zinb) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()


# ============================================================
# STEP 8 – Monte Carlo prediction intervals (best model)
# ============================================================
# Use ZINB (typically best for overdispersed intermittent demand)
n_sim      <- 5000
theta_zinb <- fit_zinb$theta   # dispersion parameter

sim_matrix <- matrix(0L, nrow = n_sim, ncol = h)

for (s in seq_len(n_sim)) {
  for (t in seq_len(h)) {
    # Draw from zero-inflation component: is this a structural zero?
    structural_zero <- rbinom(1, 1, future_df$pzero_zinb[t])
    if (structural_zero == 0) {
      # Draw from negative binomial count component
      mu_t <- future_df$fc_zinb[t] / (1 - future_df$pzero_zinb[t])
      sim_matrix[s, t] <- rnbinom(1, mu = mu_t, size = theta_zinb)
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

cat("\n=== ZINB MONTE CARLO PREDICTION INTERVALS ===\n")
print(pred_intervals)


# ============================================================
# STEP 9 – Visualisations
# ============================================================
hist_df <- data.frame(fecha = dates, consumo = demand)

# --- Plot 1: All model fits vs actuals ----------------------
fit_long <- df_model %>%
  dplyr::select(fecha, consumo, fit_poisson, fit_nb, fit_zip, fit_zinb) %>%
  pivot_longer(cols = starts_with("fit_"), names_to = "model", values_to = "fitted") %>%
  mutate(model = recode(model,
                        fit_poisson = "Poisson", fit_nb = "NegBin",
                        fit_zip = "ZIP",         fit_zinb = "ZINB"))

p1 <- ggplot() +
  geom_col(data = hist_df, aes(x = fecha, y = consumo),
           fill = "#B3E5FC", alpha = 0.9, width = 25) +
  geom_line(data = fit_long,
            aes(x = fecha, y = fitted, color = model, group = model),
            linewidth = 1.0) +
  scale_color_manual(values = c(
    "Poisson" = "#757575", "NegBin" = "#1976D2",
    "ZIP"     = "#F57C00", "ZINB"   = "#E91E63")) +
  labs(
    title    = "Zero-Inflated Models – In-Sample Fit Comparison",
    subtitle = "Bars = actual | Lines = model fitted values",
    x = "Date", y = "Consumption", color = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p1)


# --- Plot 2: ZINB forecast fan ------------------------------
p2 <- ggplot() +
  geom_col(data = hist_df, aes(x = fecha, y = consumo),
           fill = "#B3E5FC", alpha = 0.85, width = 25) +
  geom_ribbon(data = pred_intervals,
              aes(x = fecha, ymin = p10, ymax = p90),
              fill = "#E91E63", alpha = 0.12) +
  geom_ribbon(data = pred_intervals,
              aes(x = fecha, ymin = p25, ymax = p75),
              fill = "#E91E63", alpha = 0.22) +
  geom_line(data = pred_intervals, aes(x = fecha, y = mean),
            color = "#E91E63", linewidth = 1.3, linetype = "dashed") +
  geom_point(data = pred_intervals, aes(x = fecha, y = mean),
             color = "#E91E63", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2026-04-15")),
             linetype = "dotted", color = "grey50") +
  annotate("text", x = as.Date("2026-04-20"), y = max(demand) * 0.95,
           label = "Forecast →", hjust = 0, color = "grey50", size = 3.5) +
  labs(
    title    = "ZINB Forecast – 6 Month Horizon",
    subtitle = "Bands: 50% and 80% prediction intervals | Pink: mean forecast",
    x = "Date", y = "Consumption"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p2)


# --- Plot 3: AIC / BIC comparison bar chart -----------------
mc_long <- model_comparison %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "criterion", values_to = "value")

p3 <- ggplot(mc_long, aes(x = reorder(Model, value), y = value, fill = Model)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  facet_wrap(~criterion, scales = "free_x") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Poisson" = "#757575", "NegBin" = "#1976D2",
    "ZIP"     = "#F57C00", "ZINB"   = "#E91E63")) +
  labs(
    title    = "Model Selection – AIC & BIC",
    subtitle = "Lower is better",
    x = NULL, y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p3)


# --- Plot 4: P(structural zero) over forecast horizon -------
pzero_df <- bind_rows(
  df_model %>% select(fecha, pzero_zip, pzero_zinb) %>%
    rename(ZIP = pzero_zip, ZINB = pzero_zinb) %>%
    mutate(period = "Historical"),
  future_df %>% select(fecha, pzero_zip, pzero_zinb) %>%
    rename(ZIP = pzero_zip, ZINB = pzero_zinb) %>%
    mutate(period = "Forecast")
) %>%
  pivot_longer(cols = c(ZIP, ZINB), names_to = "model", values_to = "p_zero")

p4 <- ggplot(pzero_df, aes(x = fecha, y = p_zero * 100,
                           color = model, linetype = period)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey60") +
  scale_color_manual(values = c("ZIP" = "#F57C00", "ZINB" = "#E91E63")) +
  scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed")) +
  labs(
    title    = "P(Structural Zero) – Zero-Inflation Component",
    subtitle = "Probability that demand is absent due to structural reasons",
    x = "Date", y = "P(zero) %", color = "Model", linetype = "Period"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p4)


# --- Plot 5: Residuals comparison ---------------------------
resid_df <- df_model %>%
  mutate(
    resid_zip  = consumo - fit_zip,
    resid_zinb = consumo - fit_zinb
  ) %>%
  select(fecha, resid_zip, resid_zinb) %>%
  pivot_longer(-fecha, names_to = "model", values_to = "residual") %>%
  mutate(model = recode(model, resid_zip = "ZIP", resid_zinb = "ZINB"))

p5 <- ggplot(resid_df, aes(x = fecha, y = residual, fill = residual > 0)) +
  geom_col(alpha = 0.8, width = 25, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey40") +
  facet_wrap(~model, ncol = 1) +
  scale_fill_manual(values = c("TRUE" = "#43A047", "FALSE" = "#E53935")) +
  labs(
    title    = "Residuals: ZIP vs ZINB",
    subtitle = "Green = under-predicted | Red = over-predicted",
    x = "Date", y = "Actual – Fitted"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p5)

cat("\n=== DONE ===\n")
