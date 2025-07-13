# Random Forest for Conditional Density Estimation in R
# This example shows how to estimate f(y|x) using Random Forest

library(randomForest)
library(ggplot2)
library(dplyr)

# ====================================
# METHOD 1: Direct Density Estimation
# ====================================

# Function to estimate conditional density using Random Forest
estimate_conditional_density <- function(X_train, Y_train, X_test, Y_grid, bandwidth = 0.1) {
  
  # Create training data for density estimation
  # For each training point, create multiple (x,y) pairs with density values
  train_data <- data.frame()
  
  for(i in 1:length(X_train)) {
    # For each training point, evaluate density at grid points
    x_val <- X_train[i]
    y_val <- Y_train[i]
    
    # Create density values using kernel density estimation
    # Higher density near the actual y value
    for(y_grid_point in Y_grid) {
      density_val <- dnorm(y_grid_point, mean = y_val, sd = bandwidth)
      train_data <- rbind(train_data, 
                         data.frame(x = x_val, y = y_grid_point, density = density_val))
    }
  }
  
  # Train Random Forest to predict density values
  rf_model <- randomForest(density ~ x + y, data = train_data, ntree = 100)
  
  # Predict densities for test points
  test_data <- expand.grid(x = X_test, y = Y_grid)
  test_data$predicted_density <- predict(rf_model, test_data)
  
  return(list(model = rf_model, predictions = test_data))
}

# ====================================
# METHOD 2: Quantile-based Approach
# ====================================

# Function to estimate density via quantile regression
estimate_density_via_quantiles <- function(X_train, Y_train, X_test, n_quantiles = 20) {
  
  quantile_levels <- seq(0.05, 0.95, length.out = n_quantiles)
  quantile_preds <- matrix(NA, nrow = length(X_test), ncol = n_quantiles)
  
  # Estimate quantiles using Random Forest
  for(i in 1:n_quantiles) {
    # Create weighted training data for quantile regression
    tau <- quantile_levels[i]
    
    # Simple approach: use all data but weight by quantile loss
    train_df <- data.frame(x = X_train, y = Y_train)
    
    # Train RF (note: this is simplified - proper quantile RF would use quantregForest)
    rf_q <- randomForest(y ~ x, data = train_df, ntree = 100)
    quantile_preds[, i] <- predict(rf_q, data.frame(x = X_test))
  }
  
  # Convert quantiles to density estimates
  densities <- list()
  for(i in 1:length(X_test)) {
    q_vals <- quantile_preds[i, ]
    # Density is inverse of quantile spacing
    density_approx <- 1 / diff(c(q_vals[1], q_vals, q_vals[length(q_vals)]))
    densities[[i]] <- density_approx[1:n_quantiles]
  }
  
  return(list(quantiles = quantile_preds, densities = densities, levels = quantile_levels))
}

# ====================================
# EXAMPLE: Generate Data and Test
# ====================================

set.seed(42)

# Generate heteroscedastic data (variance depends on x)
n_train <- 200
n_test <- 50

X_train <- runif(n_train, -3, 3)
# Conditional mean and variance depend on x
Y_train <- 2 * X_train + rnorm(n_train, sd = 1 + abs(X_train))

X_test <- seq(-3, 3, length.out = n_test)
Y_grid <- seq(-10, 15, length.out = 50)

plot(X_train,Y_train)
hist(Y_train)

# ====================================
# Apply Method 1: Direct Density Estimation
# ====================================

cat("Estimating conditional density using Random Forest...\n")
density_result <- estimate_conditional_density(X_train, Y_train, X_test, Y_grid)

# ====================================
# Visualization
# ====================================

# Prepare data for plotting
plot_data <- density_result$predictions
plot_data$x_discrete <- factor(round(plot_data$x, 1))

# Create heatmap of estimated densities
p1 <- ggplot(plot_data, aes(x = x, y = y, fill = predicted_density)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Density") +
  geom_point(data = data.frame(x = X_train, y = Y_train), 
             aes(x = x, y = y), fill = NA, color = "red", alpha = 0.5, size = 0.8) +
  labs(title = "Conditional Density Estimation using Random Forest",
       subtitle = "Red points are training data",
       x = "X", y = "Y") +
  theme_minimal()

print(p1)

# ====================================
# Example: Extract density for specific x values
# ====================================

# Function to get density estimate for specific (x,y) point
get_density_at_point <- function(x_val, y_val, model) {
  test_point <- data.frame(x = x_val, y = y_val)
  return(predict(model, test_point))
}

# Example usage
x_example <- 1.0
y_example <- 3.0
density_at_point <- get_density_at_point(x_example, y_example, density_result$model)

cat(sprintf("Estimated density at (x=%.1f, y=%.1f): %.4f\n", 
            x_example, y_example, density_at_point))

# ====================================
# Method 2 Example (using quantregForest if available)
# ====================================

# If you have quantregForest package:
# library(quantregForest)
# 
# qrf_model <- quantregForest(x = matrix(X_train), y = Y_train)
# quantile_preds <- predict(qrf_model, matrix(X_test), quantiles = seq(0.1, 0.9, 0.1))

cat("\nDone! The Random Forest has learned to map (x,y) -> density values.\n")
cat("This can now be used in conformal prediction methods like Dist-split.\n")

# ====================================
# Simple Dist-split Implementation
# ====================================

# Simple implementation of Dist-split concept
simple_dist_split <- function(X_train, Y_train, X_test, alpha = 0.1) {
  
  # Step 1: Estimate conditional CDF by integrating density
  Y_grid <- seq(min(Y_train) - 2*sd(Y_train), max(Y_train) + 2*sd(Y_train), length.out = 100)
  
  # Get density estimates for training data
  cdf_values <- numeric(length(X_train))
  
  for(i in 1:length(X_train)) {
    # Get density estimates for this x value
    test_points <- data.frame(x = rep(X_train[i], length(Y_grid)), y = Y_grid)
    densities <- predict(density_result$model, test_points)
    
    # Approximate CDF by integration (trapezoidal rule)
    dy <- diff(Y_grid)[1]
    cdf_approx <- cumsum(densities * dy)
    
    # Find CDF value at observed Y
    y_idx <- which.min(abs(Y_grid - Y_train[i]))
    cdf_values[i] <- cdf_approx[y_idx]
  }
  
  # Step 2: Get quantiles of these CDF values
  lower_q <- quantile(cdf_values, alpha/2)
  upper_q <- quantile(cdf_values, 1 - alpha/2)
  
  cat(sprintf("Dist-split quantiles: [%.3f, %.3f]\n", lower_q, upper_q))
  
  return(list(lower_quantile = lower_q, upper_quantile = upper_q, cdf_values = cdf_values))
}

# Apply simple Dist-split
dist_split_result <- simple_dist_split(X_train, Y_train, X_test)

cat("This demonstrates the basic principle of using RF for density estimation in conformal prediction!")