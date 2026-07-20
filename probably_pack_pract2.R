#Practice ML
#------------
library(dplyr)
library(caret)
library(nestedcv)
library(glmnet)
library(ggplot2)
library(tidymodels)
library(rsample)
library(purrr)
library(probably)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# generate data
set.seed(3212)
n<-800
alpha<-0.05
z_lo <- qnorm(alpha / 2)          # negative
z_hi <- qnorm(1 - alpha / 2)      # positive

x         <- runif(n, 0, 5)
mu        <- 2 * sin(1.5 * x)
sigma     <- 0.2 + 0.35 * x                 # grows with x
eps       <- rnorm(n)
y         <- mu + sigma * eps
oracle_lo <- mu + sigma * z_lo
oracle_hi <- mu + sigma * z_hi
out <- data.frame(x, y, mu_true = mu, sigma_true = sigma,
                  oracle_lo, oracle_hi)

# train/test set
set.seed(99)
splits     <- initial_validation_split(out, prop = c(0.5, 0.25))
train_data <- training(splits)
cal_data   <- validation(splits)
test_data  <- testing(splits)

# fit model
spline_rec <- recipe(y ~ x, data = train_data) |>
              step_ns(x, deg_free = 10) # modern equivalent: step_spline_natural(x, deg_free = 10)
spline_wflow <- workflow() |>
                add_recipe(spline_rec) |>
                add_model(linear_reg())
spline_fit <- fit(spline_wflow, data = train_data)

train_pred <- augment(spline_fit, new_data = train_data)
ggplot(train_pred, aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.4) +          # real data
  geom_line(aes(y = .pred), color = "blue", linewidth = 1) +  # fitted spline
  labs(y = "y", title = "Spline fit vs real data")


# ----  Split conformal (constant-width intervals) -----------------
split_int <- int_conformal_split(spline_fit, cal_data)
res_split <- predict(split_int, test_data, level = 1 - alpha) |>
             bind_cols(test_data) |>
             arrange(x)
ggplot(res_split, aes(x = x)) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper),
              fill = "blue", alpha = 0.2) +          # conformal band
  geom_line(aes(y = .pred), color = "blue", linewidth = 1) +  # point prediction
  geom_point(aes(y = y), alpha = 0.4) +              # real test data
  labs(y = "y",
       title = paste0("Split conformal interval (", (1 - alpha) * 100, "% coverage)"))

# ---- Conformalized quantile regression (adaptive width) ---------
# Needs BOTH train and calibration data; level is fixed here, not at predict.
quant_int <- int_conformal_quantile(
  spline_fit,
  train_data = train_data,
  cal_data   = cal_data,
  level      = 1 - alpha,
  ntree      = 2000
)
res_quant <- predict(quant_int, test_data) |>
             bind_cols(test_data) |>
             arrange(x)
ggplot(res_quant, aes(x = x)) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper),
              fill = "blue", alpha = 0.2) +          # conformal band
  geom_line(aes(y = .pred), color = "blue", linewidth = 1) +  # point prediction
  geom_point(aes(y = y), alpha = 0.4) +              # real test data
  labs(y = "y",
       title = paste0("Split conformal interval (", (1 - alpha) * 100, "% coverage)"))


### Coverage
coverage <- function(df, label) {
  df |>
    summarise(
      method     = label,
      coverage   = mean(.pred_lower <= y & y <= .pred_upper) * 100,
      mean_width = mean(.pred_upper - .pred_lower)
    )
}
bind_rows(
  coverage(res_split, "Split conformal"),
  coverage(res_quant, "Conformalized quantile")
)


# ---- 6. Coverage by region: where the methods differ ----------------
# This exposes the heteroscedastic failure of the constant-width method.
binned <- function(df, label) {
  df |>
    mutate(x_bin = cut(x, breaks = 0:5)) |>
    group_by(x_bin) |>
    summarise(
      coverage   = mean(.pred_lower <= y & y <= .pred_upper) * 100,
      mean_width = mean(.pred_upper - .pred_lower),
      .groups = "drop"
    ) |>
    mutate(method = label)
}
bind_rows(binned(res_split, "Split conformal"),
          binned(res_quant, "Conformalized quantile")) |>
  arrange(x_bin, method) |>
  print(n = Inf)
# Expect: split conformal OVER-covers at low x (too wide) and
# UNDER-covers at high x (too narrow). Quantile method stays ~95% throughout.

# ---- 7. Visual comparison vs oracle (dashed red) --------------------
bind_rows(
  res_split |> mutate(method = "Split conformal"),
  res_quant |> mutate(method = "Conformalized quantile")
) |>
  ggplot(aes(x)) +
  geom_point(aes(y = y), alpha = 0.15) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper),
              fill = "steelblue", alpha = 0.30) +
  geom_line(aes(y = oracle_lo), colour = "red", linetype = 2) +
  geom_line(aes(y = oracle_hi), colour = "red", linetype = 2) +
  facet_wrap(~ method) +
  labs(
    title    = "95% conformal prediction intervals vs. oracle (dashed red)",
    subtitle = "Blue band = conformal interval; noise grows with x",
    y = "y"
  ) +
  theme_minimal()
