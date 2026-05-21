library(tidymodels)
library(tidyverse)
library(bonsai)

set.seed(42)
n <- 500

# Simulate data with non-linear interactions — territory where RF and GBM differ
df <- tibble(
  x1 = rnorm(n), x2 = rnorm(n), x3 = runif(n, 0, 10),
  x4 = rnorm(n), x5 = rbinom(n, 1, 0.5),
  y  = 3*x1^2 + sin(x2*pi) + 0.5*x3 + 2*x4*x5 + rnorm(n, sd = 2)
)

# Shared folds — 10-fold × 3 repeats = 30 paired observations
cv_folds <- vfold_cv(df, v = 10, repeats = 3)

# Shared recipe
rec <- recipe(y ~ ., data = df) |>
       step_normalize(all_numeric_predictors())

# Random Forest spec
spec_rf <- rand_forest(trees = 500, mtry = 3, min_n = 5) |>
  set_engine("ranger") |>
  set_mode("regression")

# GBM spec (LightGBM)
spec_gbm <- boost_tree(trees = 500, learn_rate = 0.05,
                       tree_depth = 4, min_n = 10) |>
  set_engine("lightgbm") |>
  set_mode("regression")

# Workflows
wf_rf  <- workflow() |> add_recipe(rec) |> add_model(spec_rf)
wf_gbm <- workflow() |> add_recipe(rec) |> add_model(spec_gbm)

# Fit on same folds, track R², RMSE, MAE
metrics <- metric_set(rsq, rmse, mae)
res_rf  <- fit_resamples(wf_rf,  resamples = cv_folds, metrics = metrics)
res_gbm <- fit_resamples(wf_gbm, resamples = cv_folds, metrics = metrics)

# Extract per-fold scores for each metric
fold_scores <- function(res, model_name) {
  collect_metrics(res, summarize = FALSE) |>
    select(id, id2, .metric, .estimate) |>
    mutate(model = model_name)
}

scores <- bind_rows(
  fold_scores(res_rf,  "Random Forest"),
  fold_scores(res_gbm, "GBM")
)

# Mann-Whitney U (paired) for each metric
run_test <- function(metric_name) {
  a <- scores |> filter(model == "Random Forest", .metric == metric_name) |>
    arrange(id, id2) |> pull(.estimate)
  b <- scores |> filter(model == "GBM", .metric == metric_name) |>
    arrange(id, id2) |> pull(.estimate)
  
  t <- wilcox.test(a, b, paired = TRUE, conf.int = TRUE)
  r <- abs(1 - (2 * t$statistic) / length(a)^2)
  
  tibble(
    metric      = metric_name,
    mean_rf     = mean(a) |> round(4),
    mean_gbm    = mean(b) |> round(4),
    W           = t$statistic,
    p_value     = t$p.value |> round(4),
    effect_r    = r |> round(3),
    significant = t$p.value < 0.05,
    winner      = case_when(
      t$p.value >= 0.05           ~ "no difference",
      metric_name == "rsq" & mean(a) > mean(b) ~ "Random Forest",
      metric_name == "rsq" & mean(b) > mean(a) ~ "GBM",
      metric_name != "rsq" & mean(a) < mean(b) ~ "Random Forest",
      TRUE                                       ~ "GBM"
    )
  )
}

results <- map_dfr(c("rsq", "rmse", "mae"), run_test)
print(results)

# Visualise fold distributions
scores |>
  mutate(.metric = factor(.metric, levels = c("rsq","rmse","mae"))) |>
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_boxplot(width = 0.45, alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.1, size = 1.2, alpha = 0.4) +
  facet_wrap(~.metric, scales = "free_y") +
  labs(title = "RF vs GBM — per-fold CV distributions",
       x = NULL, y = "Score") +
  theme_minimal() +
  theme(legend.position = "none")


