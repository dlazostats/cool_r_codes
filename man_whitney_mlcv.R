library(tidyverse)
library(caret)

set.seed(42)
n <- 300

# Simulate data
df <- tibble(
  x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
  y  = 2*x1 - 1.5*x2 + rnorm(n, sd = 1.5)
)

# 10-fold CV control — same folds for both models
folds <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                      savePredictions = "final")

# Model A: linear regression
model_a <- train(y ~ x1 + x2 + x3, data = df,
                 method    = "lm",
                 trControl = folds)

# Model B: ridge regression
model_b <- train(y ~ x1 + x2 + x3, data = df,
                 method    = "ridge",
                 trControl = folds)

# Extract per-fold R² scores
scores_a <- model_a$resample$Rsquared
scores_b <- model_b$resample$Rsquared

{
  cat("Model A per-fold R²:", round(scores_a, 3), "\n")
  cat("Model B per-fold R²:", round(scores_b, 3), "\n")
  cat(sprintf("\nMean R²  — A: %.4f  B: %.4f\n",
              mean(scores_a), mean(scores_b)))  
}

# Are the fold score distributions significantly different?
test <- wilcox.test(scores_a, scores_b,
                    paired      = TRUE,   # same folds → paired!
                    alternative = "two.sided",
                    conf.int    = TRUE)

{
  cat(sprintf("W statistic : %.0f\n", test$statistic))
  cat(sprintf("p-value     : %.4f\n", test$p.value))
  cat(sprintf("Location shift CI: [%.4f, %.4f]\n",
              test$conf.int[1], test$conf.int[2]))  
}

# Effect size
r <- abs(1 - (2 * test$statistic) / (length(scores_a) * length(scores_b)))
cat(sprintf("Effect size r: %.3f\n", r))

if (test$p.value < 0.05) {
  winner <- ifelse(mean(scores_a) > mean(scores_b), "Model A", "Model B")
  cat(sprintf("\n%s is significantly better (p = %.4f)\n",
              winner, test$p.value))
} else {
  cat("\nNo significant difference between models.",
      "Choose based on complexity/interpretability.\n")
}
