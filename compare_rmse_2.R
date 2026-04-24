# Compare ml models 2
#----------------------
library(tidymodels)
library(ggplot2)
library(tidyposterior)
library(ranger)
library(plotly)
library(xgboost)
library(janitor)
library(boot)
library(lattice)

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
folds <- vfold_cv(df, v = 10, repeats = 5)  

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

# ── 5. Fit models ──────────────────────────────────────────────────────────────
rf_res <- fit_resamples(
  rf_wf, 
  resamples = folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = TRUE)
)

xgb_res <- fit_resamples(
  xgb_wf, 
  resamples = folds,
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

scores <- bind_cols(rf_scores, xgb_scores) %>% as.data.frame() %>% clean_names() %>% 
          mutate(dif=estimate_5-estimate_12)

scores_dif<-scores %>% select(dif)

dif_rf_xg<-function(data,indx){
  df0<-data[indx,]
  dif_rf_xg<-df0$estimate_5-df0$estimate_12
  return(mean(dif_rf_xg))
}
boot_d<-boot(scores,dif_rf_xg,R=10000)
boot_d
boot.ci(boot_d)
hist(boot_d$t,breaks="FD")
histogram(boot_d$t)

dtf<-data.frame(t=boot_d$t)
ggplot(dtf,aes(x=t))+
  geom_freqpoly()
plot_ly(data = dtf, x = ~t, type = "histogram",
        marker = list(
          line = list(
            color = "white",  # border color
            width = 1         # border thickness
          )
        ))
plot_ly(type = "histogram") 

#Probability statement
library(bayesboot)
library(bayestestR)

bb <- bayesboot(scores, dif_rf_xg, R = 10000)
describe_posterior(bb$V1,
                   ci_method = "HDI",
                   rope_range = c(-0.01, 0.01))

# Check your scale
range(scores$estimate_5)
range(scores$estimate_12)
sd(scores$estimate_5 - scores$estimate_12)
sdf<-sd(scores$estimate_5 - scores$estimate_12)

describe_posterior(bb$V1,
                   ci_method = "HDI",
                   rope_range = c(-0.1*sdf, 0.1*sdf))

#--------------------------------------------------------------------------------------------------------------------
# Run both and compare directly
dif <- scores$estimate_5 - scores$estimate_12

# Classical
ci_classical <- quantile(
  replicate(10000, mean(sample(dif, replace = TRUE))),
  c(0.025, 0.975)
)

# Bayesian
bb  <- bayesboot(dif, mean, R = 10000)
ci_bayesian <- quantile(bb$V1, c(0.025, 0.975))

cat("Classical 95% CI:", round(ci_classical, 4), "\n")
cat("Bayesian  95% CI:", round(ci_bayesian,  4), "\n")
cat("P(diff < 0 | data):", round(mean(bb$V1 < 0), 3), "\n")
