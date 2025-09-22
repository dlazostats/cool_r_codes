# Libraries
library(dplyr)
library(tidymodels)
library(doParallel)
library(DALEX)

# set working directory
script_name <- 'ml_workflow.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
dt0<-read.csv("train_qtb.csv")
dt1<-dt0 %>% dplyr::select(FLUENCIA,PE_MET:material_c)

# train/test/val set
split<-initial_split(dt1,strata=FLUENCIA)
train<-training(split)
test<-testing(split)

# workflow
kcv <- vfold_cv(train,v=5)
rf_recipe <- recipe(FLUENCIA ~ ., 
                    data = train) %>%
             step_normalize(all_numeric_predictors())
rf_spec <- rand_forest(mtry = tune(),           # number of variables to randomly sample at each split
                       trees = tune(),          # number of trees
                       min_n = tune()) %>%      # minimum number of data points in a node
           set_engine("ranger", importance = "permutation") %>%
           set_mode("regression")
rf_workflow <- workflow() %>%
               add_recipe(rf_recipe) %>%
               add_model(rf_spec)
rf_params <- extract_parameter_set_dials(rf_workflow) %>% 
             update(mtry = finalize(mtry(), train)) 

registerDoParallel()
grid_fit <- tune_bayes(
  rf_workflow, 
  resamples = kcv,
  param_info = rf_params,
  initial = 25, 
  iter    = 25, 
  metrics = metric_set(rmse), 
  control = control_bayes(no_improve = 20, verbose = TRUE)
)
stopImplicitCluster()
grid_fit %>% show_best(metric = "rmse") 

rf_best_param<-grid_fit %>% select_best(metric="rmse")
rf_model_final <- rf_spec %>% finalize_model(rf_best_param)

## Train error
prep_recp <- recipe(FLUENCIA ~ .,data = training(split)) %>% 
             step_normalize(all_predictors()) %>%
             prep()
train_processed <- bake(prep_recp,new_data = training(split))
train_prediction <- rf_model_final %>%
                    fit(formula = FLUENCIA ~ ., 
                        data    = train_processed) %>%
                    predict(new_data = train_processed) %>%
                    bind_cols(train)
rf_score_train <- train_prediction %>%
                  metrics(FLUENCIA, .pred) %>%
                  mutate(.estimate = format(round(.estimate, 2),
                                            big.mark = ","))
rf_score_train

## Test
test_processed <- bake(prep_recp,new_data =  testing(split))
test_prediction <- rf_model_final %>%
                   fit(formula = FLUENCIA ~ ., 
                       data    = test_processed) %>%
                   predict(new_data = test_processed) %>%
                   bind_cols(testing(split))
rf_score_test <- test_prediction %>%
                 metrics(FLUENCIA , .pred) %>%
                 mutate(.estimate = format(round(.estimate, 2),
                                          big.mark = ","))
rf_score_test

# Final performance
registerDoSEQ()

## using Tidymodels
final_rf <- finalize_workflow(rf_workflow,rf_best_param)
final_rf
final_fit <- last_fit(final_rf, split)

final_fit %>% collect_metrics()
fin_pred<-final_fit %>% collect_predictions()

plot(test$FLUENCIA,type="l")
lines(fin_pred$.pred,col="red")

## using dalex
rf_xtr_wflow<-extract_workflow(final_fit)  
rf_explain<-DALEX::explain(rf_xtr_wflow,data=test,y=test$FLUENCIA,
                           predict_function = function(model, newdata) {
                             predict(model, newdata)$.pred
                           })
model_performance(rf_explain)
plot(model_performance(rf_explain))

## Save model
res_bundle <-final_fit %>%            #<- changed
  extract_workflow() %>%   #<- changed
  butcher() %>% 
  bundle()
saveRDS(res_bundle,"xgb_flue_notemp.RDS")
