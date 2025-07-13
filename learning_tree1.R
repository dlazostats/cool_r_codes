library(ISLR)
library(dplyr)
library(tree)
library(rpart.plot)
library(caret)
library(tidymodels)
library(parttree)
library(rpart)
library(vip)
library(ggplot2)
library(partykit)

# Simple example
#---------------
data(Carseats)
head(Carseats)
rpartTree <- rpart(Sales ~ CompPrice+Income,  
                   method = "anova",
                   data = Carseats )
rpartTree2 <- as.party(rpartTree)
plot(rpartTree2)
rpart.plot(rpartTree, roundint = FALSE, type = 3, branch = .3)

# Tunning example
#----------------
## Train/Test
set.seed(123)
split <- initial_split(Carseats, strata = Sales)
train <- training(split)
test <- testing(split)

## CV
set.seed(234)
cvfolds <- vfold_cv(train, strata = Sales)
cvfolds

## Model specification
tree_spec <- decision_tree(cost_complexity = tune(),
                           tree_depth = tune(),
                           min_n = tune()) %>%
             set_engine("rpart") %>%
             set_mode("regression")
tree_spec

## tune grid
tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid

## Tune Hyperparameters
doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tune_grid(tree_spec,
                     Sales ~ .,
                     resamples = cvfolds,
                     grid = tree_grid,
                     metrics = metric_set(rmse, rsq, mae, mape))
tree_rs

## Evaluate model
collect_metrics(tree_rs)
autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")
show_best(tree_rs, "rmse")

## Final tree
final_tree <- finalize_model(tree_spec, 
                             select_best(tree_rs, "rmse"))
final_tree

## Final fit
final_fit <- fit(final_tree, Sales ~ .,train)
final_rs <- last_fit(final_tree, Sales ~ ., split)
final_rs

## Variable importance
final_fit %>%
  vip(geom = "col", 
      aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

ex_fit <- fit(final_tree,
              Sales ~ Price + CompPrice,
              train)

train %>%
  ggplot(aes(Price, CompPrice)) +
  geom_parttree(data = ex_fit, aes(fill = Sales), alpha = 0.3) +
  geom_jitter(alpha = 0.7, width = 1, height = 0.5, aes(color = Sales)) +
  scale_colour_viridis_c(aesthetics = c("color", "fill"))

## Final metrics
collect_metrics(final_rs)

final_rs %>%
  collect_predictions() %>%
  ggplot(aes(Sales, .pred)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  coord_fixed()
