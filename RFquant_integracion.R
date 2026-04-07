# install.packages("quantregForest")  # if not installed
library(quantregForest)

# Example data
set.seed(123)
n <- 300
x1 <- runif(n, 0, 5)
y  <- 3 + 2 * sin(x1) + rnorm(n, 0, 0.5 + 0.2*x1)  # heteroskedastic noise
X  <- data.frame(x1 = x1)

# Fit quantile random forest
qrf <- quantregForest(x = X, y = y, ntree = 1000)

# Define grid of quantiles
taus <- seq(0.01, 0.99, by = 0.01)

# Predict conditional quantiles for all observations (or new data)
Qhat <- predict(qrf, newdata = X, what = taus)
# Qhat: matrix [nrow(X) x length(taus)]

# --- Trapezoidal integration function ---
trapz <- function(x, y) {
  # Integrate y over x using the trapezoidal rule
  sum(0.5 * diff(x) * (head(y, -1) + tail(y, -1)))
}

# --- Compute conditional mean, variance, and sd ---
mu_hat  <- apply(Qhat, 1, function(q) trapz(taus, q))              # E[Y|X=x]
E2_hat  <- apply(Qhat, 1, function(q) trapz(taus, q^2))            # E[Y^2|X=x]
var_hat <- pmax(E2_hat - mu_hat^2, 0)                              # Var[Y|X=x]
sd_hat  <- sqrt(var_hat)                                           # SD[Y|X=x]

# Store results
result_df <- data.frame(
  x1 = X$x1,
  mean = mu_hat,
  sd = sd_hat
)

# --- Visualize conditional mean and SD ---
library(ggplot2)
ggplot(result_df, aes(x = x1)) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  geom_line(aes(y = mean + sd), color = "red", linetype = "dashed") +
  geom_line(aes(y = mean - sd), color = "red", linetype = "dashed") +
  labs(title = "Conditional Mean and ±1 SD from Quantile Random Forest",
       y = "Predicted Mean ± SD") +
  theme_minimal()
