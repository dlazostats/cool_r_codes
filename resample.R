library(resample)
library(tidymodels)
loo<-loo_cv(mtcars)
dim(loo)
dim(mtcars)

set.seed(1212)
split<-initial_split(mtcars,prop=0.8)
train<-training(split)
test<-testing(split)
cv<-vfold_cv(mtcars,8)
bt<-bootstrap(mtcars,times=25)

lm_spec<-linear_reg() %>% set_engine("lm")
results<-fit_resamples(
  lm_spec,
  mpg~disp+hp+wt,
  resamples = vfold_cv(mtcars,v=5),
  metrics=metric_set(rmse,rsq)
)
collect_metrics(results)

# boostrap resamples
boot_mpg<-bootstraps(mtcars,times=25)
boot_sum<-boot_mpg %>% 
          mutate(mean_mpg=map_dbl(splits,~mean(analysis(.x)$mpg)))
sd(boot_sum$mean_mpg) #bootstrap se

# Look at the folds
cv_folds <- vfold_cv(mtcars, v = 5)
cv_folds
cv_folds$splits[[1]] %>% analysis() 
cv_folds$splits[[1]] %>% assessment()

res_fit <- fit_resamples(
  lm_spec,
  mpg ~ wt + hp,
  resamples = cv_folds
)
collect_metrics(res_fit)
collect_metrics(res_fit,summarize = F)

# custom metrics
custom_metrics <- metric_set(rmse, rsq, mae)
fit_resamples(lm_spec, mpg ~ wt + hp, resamples = cv_folds, metrics = custom_metrics)

dtt<-res_fit %>% 
  collect_metrics(summarize = FALSE) %>% filter(.metric=="rmse")
plot(dtt$.estimate)

## tunning hyperparameters
tune_spec <- rand_forest(mtry = tune(), trees = 200) %>% set_engine("ranger") %>% set_mode("regression")

tune_res <- tune_grid(
  tune_spec,
  mpg ~ .,
  resamples = cv_folds,
  grid = 10
)
show_best(tune_res)

# Boot CI
cor(mtcars$mpg,mtcars$wt)
boot_res <- bootstraps(mtcars, times = 25)
boot_res
boot_results <- boot_res %>%
  mutate(results = map(splits, ~ cor(analysis(.x)$mpg, analysis(.x)$wt)))
boot_results %>% summarize(sd_cor = sd(unlist(results)))








