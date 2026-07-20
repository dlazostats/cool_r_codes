library(workflows)
library(dplyr)
library(parsnip)
library(rsample)
library(tune)
library(modeldata)

set.seed(2)
sim_train <- sim_regression(500)
sim_cal <- sim_regression(200)
sim_new <- sim_regression(50) |> select(-outcome)
sim_new <- sim_regression(50) 

# We'll use a neural network model
mlp_spec <-
  mlp(hidden_units = 5, penalty = 0.01) |>
  set_mode("regression")

mlp_wflow <-
  workflow() |>
  add_model(mlp_spec) |>
  add_formula(outcome ~ .)

mlp_fit <- fit(mlp_wflow, data = sim_train)

mlp_int <- int_conformal_quantile(mlp_fit, sim_train, sim_cal,
                                  level = 0.90
)
mlp_int

prdd<-predict(mlp_int, sim_new)
plot(sim_new$outcome,type="l")
lines(prdd$.pred,col="red")
lines(prdd$.pred_upper,col="blue")
lines(prdd$.pred_lower,col="blue")
