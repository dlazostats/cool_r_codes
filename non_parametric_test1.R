library(tidyverse)

set.seed(7)
n <- 200

df <- tibble(
  feature  = sample(c("A", "B"), n, replace = TRUE),
  y        = ifelse(feature == "B", rnorm(n, 8, 2), rnorm(n, 5, 2))
)

ggplot(df, aes(x = y, fill = feature)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  theme_minimal()

ggplot(df, aes(x = y, color = feature, fill = feature)) +
  geom_density(alpha = 0.3) +
  theme_minimal()

# Does feature split y meaningfully?
test <- wilcox.test(y ~ feature, data = df, conf.int = TRUE)

{
  cat(sprintf("W statistic : %.0f\n",   test$statistic))
  cat(sprintf("p-value     : %.4f\n",   test$p.value))
  cat(sprintf("Location shift (95%% CI): [%.2f, %.2f]\n",
              test$conf.int[1], test$conf.int[2]))
}

# Effect size: rank-biserial correlation
# r = 1 - (2W / n_A*n_B)
n_a <- sum(df$feature == "A")
n_b <- sum(df$feature == "B")
r   <- 1 - (2 * test$statistic) / (n_a * n_b)
cat(sprintf("Effect size r: %.3f\n", abs(r)))

# Effect size: rank-biserial correlation
# r = 1 - (2W / n_A*n_B)
n_a <- sum(df$feature == "A")
n_b <- sum(df$feature == "B")
r   <- 1 - (2 * test$statistic) / (n_a * n_b)
cat(sprintf("Effect size r: %.3f\n", abs(r)))

# Fit a model, then check if residuals differ by group
model <- lm(y ~ 1, data = df)   # intercept-only baseline
df    <- df |> mutate(resid = residuals(model))
resid_test <- wilcox.test(resid ~ feature, data = df)

cat(sprintf("Residual bias p-value: %.4f\n", resid_test$p.value))
# Significant p → your model treats the two groups differently

#Mann-Whitney U - goodzazo
#-----------------------------------
#test whether the distribution of scores across folds is meaningfully different between two models

























