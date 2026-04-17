# Install packages
#install.packages(c("tidymodels", "ranger", "xgboost", "BayesFactor", "bayestestR", "see"))

library(tidymodels)
library(ggdist)
library(ranger)
library(xgboost)
library(BayesFactor)
library(bayestestR)
library(see)
library(BSDA)
library(patchwork)

# ── 1. Simulate data ──────────────────────────────────────────────────────────
set.seed(42)
n <- 500

df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n)
)
df$y <- 2 + 1.5 * df$x1 - 0.8 * df$x2 + 0.5 * df$x3 + rnorm(n)

# ── 2. Set up 10-fold CV ──────────────────────────────────────────────────────
set.seed(42)
folds <- vfold_cv(df, v = 10)

# ── 3. Define models ──────────────────────────────────────────────────────────
rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 500) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# ── 4. Define workflow ────────────────────────────────────────────────────────
recipe <- recipe(y ~ ., data = df)

rf_wf  <- workflow() %>% add_recipe(recipe) %>% add_model(rf_spec)
xgb_wf <- workflow() %>% add_recipe(recipe) %>% add_model(xgb_spec)

# ── 5. Run cross-validation ───────────────────────────────────────────────────
rf_res  <- fit_resamples(rf_wf,  resamples = folds, metrics = metric_set(rmse, rsq))
xgb_res <- fit_resamples(xgb_wf, resamples = folds, metrics = metric_set(rmse, rsq))

# Extract per-fold RMSE scores
rf_scores  <- rf_res  %>% collect_metrics(summarize = FALSE) %>% filter(.metric == "rmse") %>% pull(.estimate)
xgb_scores <- xgb_res %>% collect_metrics(summarize = FALSE) %>% filter(.metric == "rmse") %>% pull(.estimate)

# Tests if the median difference between paired folds is 0
wilcox.test(rf_scores, xgb_scores, paired = TRUE, exact = FALSE)
SIGN.test(rf_scores, xgb_scores)
wins <- ifelse(rf_scores < xgb_scores, "RF", "XGB")
table(wins)
cat("RF wins", sum(rf_scores < xgb_scores), "of 10 folds\n")

cat("RF  RMSE per fold:", round(rf_scores, 3),  "\n")
cat("XGB RMSE per fold:", round(xgb_scores, 3), "\n")

# ── 6. Bayesian paired t-test ─────────────────────────────────────────────────
# Paired because both models saw the exact same CV folds
bf_result <- ttestBF(x = rf_scores, y = xgb_scores, paired = TRUE)
print(bf_result)

# ── 7. Sample from posterior of the difference ───────────────────────────────
posterior_samples <- posterior(bf_result)
posterior_diff    <- as.data.frame(posterior_samples)

# ── 8. Summarize posterior ────────────────────────────────────────────────────
summary_diff <- describe_posterior(posterior_diff$mu, ci = 0.95)
print(summary_diff)

# ── 9. Probability that RF is better than XGBoost ────────────────────────────
# (lower RMSE = better, so we check P(difference > 0) = P(RF worse))
p_rf_better  <- mean(posterior_diff$mu < 0)   # P(RF RMSE < XGB RMSE)
p_xgb_better <- mean(posterior_diff$mu > 0)   # P(XGB RMSE < RF RMSE)

cat("\nP(RF better  than XGB):", round(p_rf_better,  3), "\n")
cat("P(XGB better than RF) :", round(p_xgb_better, 3), "\n")

# ── 10. Plot posterior of the difference ─────────────────────────────────────
plot(bf_result)   # built-in BayesFactor plot

# Or with bayestestR / see for a nicer plot:
result <- bayesfactor_parameters(posterior_diff$mu)

plot(
  estimate_density(posterior_diff$mu),
  show_intercept = TRUE
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior of RMSE difference (RF - XGB)",
    subtitle = "Negative = RF is better | Positive = XGB is better",
    x = "Difference in RMSE"
  )


## Usando tidyposterior
#----------------------------------------------------------------------------------------------------------------------------------
library(tidymodels)
library(tidyposterior)
library(ranger)
library(xgboost)
library(rstanarm)

# ── 1. Datos ──────────────────────────────────────────────────────────────────
set.seed(42)
n <- 500

df <- data.frame(
  x1 = rnorm(n), x2 = rnorm(n),
  x3 = rnorm(n), x4 = rnorm(n)
)
df$y <- 2 + 1.5 * df$x1 - 0.8 * df$x2 + 0.5 * df$x3 + rnorm(n)

# ── 2. CV folds (los mismos para ambos modelos) ───────────────────────────────
set.seed(42)
folds <- vfold_cv(df, v = 10, repeats = 5)  # repeats mejora la estimación

# ── 3. Definir modelos ────────────────────────────────────────────────────────
rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 500) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# ── 4. Workflows ──────────────────────────────────────────────────────────────
rec <- recipe(y ~ ., data = df)

rf_wf  <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)
xgb_wf <- workflow() %>% add_recipe(rec) %>% add_model(xgb_spec)

# ── 5. Correr CV ──────────────────────────────────────────────────────────────
rf_res <- fit_resamples(
  rf_wf, resamples = folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = TRUE)
)

xgb_res <- fit_resamples(
  xgb_wf, resamples = folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = TRUE)
)

rf_scores <- rf_res %>%
  collect_metrics(summarize = FALSE) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Random Forest")

xgb_scores <- xgb_res %>%
  collect_metrics(summarize = FALSE) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "XGBoost")

scores <- bind_rows(rf_scores, xgb_scores)

# ── 2. Boxplot + jitter ───────────────────────────────────────────────────────
p1 <- scores %>%
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = model), width = 0.1, size = 2.5) +
  labs(title = "RMSE per fold", x = NULL, y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none")

# ── 3. Density plot ───────────────────────────────────────────────────────────
p2 <- scores %>%
  ggplot(aes(x = .estimate, fill = model, color = model)) +
  geom_density(alpha = 0.3) +
  labs(title = "RMSE density", x = "RMSE", y = "Density") +
  theme_minimal()

# ── 4. Dot + interval (ggdist) ───────────────────────────────────────────────
p3 <- scores %>%
  ggplot(aes(x = .estimate, y = model, fill = model, color = model)) +
  stat_halfeye(alpha = 0.6, .width = c(0.66, 0.95)) +  # 66% and 95% CI
  labs(title = "RMSE distribution (66% and 95% CI)", x = "RMSE", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# ── 5. Paired difference per fold ────────────────────────────────────────────
# Shows which model wins on EACH fold
paired <- inner_join(
  rf_scores  %>% select(id, id2, rf  = .estimate),
  xgb_scores %>% select(id, id2, xgb = .estimate),
  by = c("id", "id2")
) %>%
  mutate(
    diff  = rf - xgb,                                      # positive = XGB better
    winner = ifelse(diff < 0, "Random Forest", "XGBoost")
  )

p4 <- paired %>%
  ggplot(aes(x = interaction(id2, id), y = diff, fill = winner)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels = NULL) +
  labs(
    title = "RMSE difference per fold (RF - XGB)",
    subtitle = "Negative = RF is better | Positive = XGB is better",
    x = "Fold", y = "RF - XGB"
  ) +
  theme_minimal()

# ── 6. Summary table with mean ± sd ─────────────────────────────────────────
scores %>%
  group_by(model) %>%
  summarise(
    mean_rmse = mean(.estimate),
    sd_rmse   = sd(.estimate),
    min_rmse  = min(.estimate),
    max_rmse  = max(.estimate)
  )

# ── 7. Combine all plots ──────────────────────────────────────────────────────
(p1 | p2) / (p3 | p4) +
  plot_annotation(
    title    = "Model Comparison: Random Forest vs XGBoost",
    subtitle = "10-fold CV × 5 repeats"
  )



# ── 6. Construir tabla para tidyposterior ─────────────────────────────────────
# tidyposterior necesita un tibble con una columna por modelo
# y una fila por fold/resample
rf_scores <- rf_res %>%
  collect_metrics(summarize = FALSE) %>%
  filter(.metric == "rmse") %>%
  select(id, id2, rf = .estimate)

xgb_scores <- xgb_res %>%
  collect_metrics(summarize = FALSE) %>%
  filter(.metric == "rmse") %>%
  select(id, id2, xgb = .estimate)

scores_tbl <- inner_join(rf_scores, xgb_scores, by = c("id", "id2"))
print(scores_tbl)
# # A tibble: 50 × 4
#    id       id2    rf   xgb
#    <chr>    <chr> <dbl> <dbl>
#  1 Repeat01 Fold01 1.02  1.05
#  ...

# ── 7. Modelo Bayesiano con tidyposterior ─────────────────────────────────────
# perf_mod() ajusta un modelo Bayesiano sobre los scores de CV
# usando rstanarm internamente

set.seed(42)
bayes_mod <- perf_mod(
  scores_tbl,
  prior_intercept = rstanarm::student_t(df = 1, location = 0, scale = 0.5),
  chains = 4,
  iter   = 3000,
  seed   = 42,
  refresh = 0   # silencia output de Stan
)

# ── 8. Extraer distribuciones posteriores ─────────────────────────────────────
posteriors <- tidy(bayes_mod)
print(posteriors)

# Visualizar posteriors de cada modelo
autoplot(posteriors) +
  labs(title = "Distribución posterior del RMSE por modelo")

# ── 9. Comparación directa: contraste entre modelos ──────────────────────────
# contrast_models() calcula la diferencia posterior RF - XGB
diff_posterior <- contrast_models(
  bayes_mod,
  list_1 = "rf",
  list_2 = "xgb"
)

print(diff_posterior)
summary(diff_posterior, size = 0.05)  # size = rope (región de equivalencia práctica)

# ── 10. Plot del contraste ────────────────────────────────────────────────────
autoplot(diff_posterior, size = 0.05) +
  labs(
    title    = "Diferencia posterior: RF vs XGB (RMSE)",
    subtitle = "Negativo = RF tiene menor RMSE (RF es mejor)",
    x        = "RF - XGB"
  )



























