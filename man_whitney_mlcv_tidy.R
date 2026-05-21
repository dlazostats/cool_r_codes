library(tidymodels)
library(tidyverse)

set.seed(42)
n <- 300

df <- tibble(
  x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
  y  = 2*x1 - 1.5*x2 + rnorm(n, sd = 1.5)
)

# Resampling strategy — same folds for both models
cv_folds <- vfold_cv(df, v = 10, repeats = 5)   # 30 paired scores

# Recipes
recipe_base <- recipe(y ~ x1 + x2 + x3, data = df) |>
  step_normalize(all_predictors())

# Model specs
spec_lm <- linear_reg() |>
  set_engine("lm")

spec_ridge <- linear_reg(penalty = 0.1, mixture = 0) |>
  set_engine("glmnet")

# Workflows
wf_lm <- workflow() |>
  add_recipe(recipe_base) |>
  add_model(spec_lm)

wf_ridge <- workflow() |>
  add_recipe(recipe_base) |>
  add_model(spec_ridge)

# Fit both on the same folds
res_lm    <- fit_resamples(wf_lm,    
                           resamples = cv_folds,
                           metrics   = metric_set(rsq, rmse, mae))
res_ridge <- fit_resamples(wf_ridge, 
                           resamples = cv_folds,
                           metrics   = metric_set(rsq, rmse, mae))

# Extract per-fold R²
scores_lm    <- collect_metrics(res_lm,    summarize = FALSE) |>
  filter(.metric == "rsq") |> pull(.estimate)

scores_ridge <- collect_metrics(res_ridge, summarize = FALSE) |>
  filter(.metric == "rsq") |> pull(.estimate)

# paired = TRUE because same folds were used
test <- wilcox.test(scores_lm, scores_ridge,
                    paired      = TRUE,
                    alternative = "two.sided",
                    conf.int    = TRUE)

# Effect size (rank-biserial r)
r <- abs(1 - (2 * test$statistic) / (length(scores_lm)^2))

# Tidy summary
tibble(
  model_a     = "Linear regression",
  model_b     = "Ridge regression",
  mean_r2_a   = mean(scores_lm)    |> round(4),
  mean_r2_b   = mean(scores_ridge) |> round(4),
  W           = test$statistic,
  p_value     = test$p.value       |> round(4),
  effect_size = r                  |> round(3),
  significant = test$p.value < 0.05
)

bind_rows(
  tibble(model = "Linear",  r2 = scores_lm),
  tibble(model = "Ridge",   r2 = scores_ridge)
) |>
  ggplot(aes(x = model, y = r2, fill = model)) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5) +
  labs(
    title    = "Per-fold R² distributions",
    subtitle = sprintf("Mann-Whitney U: p = %.4f", test$p.value),
    x = NULL, y = "R²"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
