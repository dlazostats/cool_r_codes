#Practice ML
#------------
library(dplyr)
library(caret)
library(nestedcv)
library(glmnet)
library(tidymodels)
library(rsample)
library(purrr)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% 
  select(RES_TRA ,PE_MET,Temp_lam,C:material_c) %>% 
  slice_sample(n=2000)
set.seed(2121)
splitt<-initial_split(db1,prop=0.8,strata=RES_TRA)
train<-training(splitt)
test<-testing(splitt)

# Tidymodels
#----------------------------------------------------------------------------------------------------------------
set.seed(123)
nested <- nested_cv(
  db1,
  outside = vfold_cv(v = 5),      # outer loop
  inside  = vfold_cv(v = 5)       # inner loop
)

rf_spec <- rand_forest(mtry = tune(),
                       min_n = tune(), 
                       trees = 500) %>%
            set_engine("ranger") %>%
            set_mode("regression")
rec <- recipe(RES_TRA ~ ., data = db1)
wf  <- workflow() %>% 
       add_model(rf_spec) %>% 
       add_recipe(rec)

score_outer <- function(outer_split) {
  inner_res <- outer_split$inner_resamples[[1]]
  tuned <- tune_grid(wf, resamples = inner_res, grid = 10,
                     metrics = metric_set(rmse))
  best  <- select_best(tuned, metric = "rmse")
  final_wf <- finalize_workflow(wf, best)
  fit_obj  <- fit(final_wf, data = analysis(outer_split$splits[[1]]))
  preds <- augment(fit_obj, assessment(outer_split$splits[[1]]))
  #rmse(preds, truth = RES_TRA, .pred)$.estimate   # replace `outcome` with your y variable
  metric_set(rmse, rsq, mae)(preds, truth = RES_TRA, estimate = .pred) %>%
    mutate(mtry = best$mtry, min_n = best$min_n)
}

run_nested_cv <- function(nested_obj) {
  per_fold <- map2(
    nested_obj$splits, nested_obj$inner_resamples,
    ~ score_outer(list(splits = list(.x), inner_resamples = list(.y)))
  )
  raw <- bind_rows(per_fold, .id = "outer_fold")
  list(
    per_fold = raw,
    summary  = raw %>%
      group_by(.metric) %>%
      summarise(mean = mean(.estimate), sd = sd(.estimate),
                n = n(), .groups = "drop")
  )
}
out <- run_nested_cv(nested)

# RESULTS
# -------------------------------------------------------------
{
  cat("\n--- Per-fold metrics ---\n")
  print(out$per_fold)
  
  cat("\n--- Nested CV estimate (report these) ---\n")
  print(out$summary)
  
  cat("\n--- Hyperparameters chosen per outer fold ---\n")
  print(distinct(out$per_fold, outer_fold, mtry, min_n))
}

# Final Model
#----------------------------------------------------------------
set.seed(123)
final_tuned <- tune_grid(wf, resamples = vfold_cv(db1, v = 5),
                         grid = 10, metrics = metric_set(rmse))
final_model <- finalize_workflow(wf, select_best(final_tuned, metric = "rmse")) %>%
               fit(data = db1)
cat("\n--- Final model fitted on full data ---\n")
print(final_model)

# best hyperparameters
best_params %>% select(mtry, min_n)
distinct(out$per_fold, outer_fold, mtry, min_n)

## Nested CV
#------------------------------------------------------------------------------------------------------------------
y<-db1$RES_TRA
x<-as.matrix(db1 %>% select(-RES_TRA))

set.seed(123)
res <- nestcv.glmnet(
  y = y,
  x = x,
  family        = "gaussian",   # <- regression
  alphaSet      = seq(0, 1, 0.1),   # tune elastic-net alpha (0 = ridge, 1 = lasso)
  n_outer_folds = 5,            # outer loop
  n_inner_folds = 5,            # inner loop
  cv.cores      = parallel::detectCores(logical = FALSE)   # parallelize
)
summary(res)
res$summary
res$outer_result  
plot_alphas(res)    # how deviance varies with alpha
plot_lambdas(res)   # how deviance varies with lambda

res <- nestcv.glmnet(y, x, family = "gaussian",
                     filterFUN = correl_filter,        # filter predictors...
                     filter_options = list(nfilter = 50))  # ...inside each outer fold





