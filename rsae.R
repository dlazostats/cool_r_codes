library(modelr)

df <- tibble::tibble(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)

m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
grid %>% add_predictions(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)

m1 <- lm(y ~ x, data = df)
df %>% add_residuals(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
df %>% spread_residuals(m1, m2)
df %>% gather_residuals(m1, m2)

mod <- lm(mpg ~ wt, data = mtcars)
mse(mod, mtcars)
rmse(mod, mtcars)
rsquare(mod, mtcars)
mae(mod, mtcars)
qae(mod, mtcars)
mape(mod, mtcars)
rsae(mod, mtcars)

library(ggplot2)

# Actual values
y_actual <- mtcars$mpg

# Predictions from your model
y_pred <- predict(mod, mtcars)

# Baseline: mean prediction
y_mean <- mean(y_actual)

# Absolute errors
err_baseline <- abs(y_actual - y_mean)
err_model <- abs(y_actual - y_pred)

# Prepare data for plotting
df <- data.frame(
  Car = rownames(mtcars),
  Baseline_Error = err_baseline,
  Model_Error = err_model
)

# Reshape for ggplot
library(tidyr)
df_long <- pivot_longer(df, cols = c("Baseline_Error", "Model_Error"),
                        names_to = "Type", values_to = "Error")

# Plot
ggplot(df_long, aes(x = reorder(Car, Error), y = Error, fill = Type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = paste0("Model vs Baseline Errors (RSAE = ", round(rsae(mod, mtcars), 4), ")"),
       x = "Car", y = "Absolute Error") +
  scale_fill_manual(values = c("Baseline_Error" = "grey70", "Model_Error" = "skyblue"),
                    labels = c("Baseline (mean prediction)", "Your model")) +
  theme_minimal()
