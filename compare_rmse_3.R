library(tidymodels)
library(tidyposterior)
library(workflowsets)

data(two_class_dat, package = "modeldata")

set.seed(100)
folds <- vfold_cv(two_class_dat)

logistic_reg_glm_spec <-
  logistic_reg() %>%
  set_engine('glm')

mars_earth_spec <-
  mars(prod_degree = 1) %>%
  set_engine('earth') %>%
  set_mode('classification')

rs_ctrl <- control_resamples(save_workflow = TRUE)

logistic_reg_glm_res <- logistic_reg_glm_spec %>%
  fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)

mars_earth_res <- mars_earth_spec %>%
  fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)

logistic_roc <-collect_metrics(logistic_reg_glm_res, summarize = FALSE) %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(id, logistic = .estimate)

mars_roc <-collect_metrics(mars_earth_res, summarize = FALSE) %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(id, mars = .estimate)

resamples_df <- full_join(logistic_roc, mars_roc, by = "id")
resamples_df

set.seed(101)
roc_model_via_df <- perf_mod(resamples_df, refresh = 0)
tidy(roc_model_via_df) %>% summary()

# using caret
library(caret)

set.seed(102)
logistic_caret <- train(Class ~ ., data = two_class_dat, method = "glm",
                        trControl = trainControl(method = "cv"))

set.seed(102)
mars_caret <- train(Class ~ ., data = two_class_dat, method = "gcvEarth",
                    tuneGrid = data.frame(degree = 1),
                    trControl = trainControl(method = "cv"))

caret_resamples <- resamples(list(logistic = logistic_caret, mars = mars_caret))

set.seed(101)
roc_model_via_caret <- perf_mod(caret_resamples, refresh = 0)
tidy(roc_model_via_caret) %>% summary()
