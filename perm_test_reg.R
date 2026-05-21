library(tidyverse)

set.seed(42)
n <- 100

# Simulate data: y has a real relationship with x1, noise from x2
df <- tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  y  = 2.5 * x1 + 0.3 * x2 + rnorm(n, sd = 1.5)
)

# Fit the real model
real_model <- lm(y ~ x1 + x2, data = df)
real_r2    <- summary(real_model)$r.squared

# Permutation test
n_perms   <- 1000
perm_r2   <- numeric(n_perms)

for (i in seq_len(n_perms)) {
  df_perm      <- df
  df_perm$y    <- sample(df$y)          # shuffle target only
  perm_model   <- lm(y ~ x1 + x2, data = df_perm)
  perm_r2[i]   <- summary(perm_model)$r.squared
}

# p-value: proportion of permuted R² >= real R²
p_value <- mean(perm_r2 >= real_r2)

cat(sprintf("Real R²:  %.4f\n", real_r2))
cat(sprintf("P-value:  %.4f\n", p_value))

cat(sprintf(
  "Linear model: R² = %.3f, permutation p-value = %.4f (n_perms = 1000)\n",
  real_r2, p_value
))

library(lmPerm)

perm_model <- lmp(y ~ x1 + x2, data = df, perm = "Prob", Ca = 0.001)
summary(perm_model)
