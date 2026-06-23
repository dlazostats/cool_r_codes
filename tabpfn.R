library(tidymodels)
library(tabpfn)
predictors <- mtcars[, -1]
outcome <- mtcars[, 1]

# XY interface
mod <- tab_pfn(predictors, outcome)
