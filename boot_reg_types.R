# types of bootstrap
#----------------------
# fit the base model
base_model <- lm(mpg ~ wt + hp, data = mtcars)
B <- 1000 # Number of bootstrap iterations

# 1.- Fully model based bootstrap (parametric)
boot_model_based <- replicate(B, {
  # Create a copy of the data for the simulation world
  sim_data <- mtcars
  
  # Extract expected values and standard error of residuals
  expected_mpg <- predict(base_model)
  sigma_hat <- summary(base_model)$sigma
  
  # Generate new response with Gaussian noise
  sim_data$mpg <- expected_mpg + rnorm(nrow(sim_data), mean = 0, sd = sigma_hat)
  
  # Re-estimate the model and return coefficients
  coef(lm(mpg ~ wt + hp, data = sim_data))
})

funplot<-function(model){
  par(mfrow = c(1, 3))
  hist(model[1, ], main = "Intercept", xlab = "Estimate", col = "lightblue")
  hist(model[2, ], main = "Coefficient: wt", xlab = "Estimate", col = "lightgreen")
  hist(model[3, ], main = "Coefficient: hp", xlab = "Estimate", col = "salmon")
  par(mfrow = c(1, 1))
}

# 2.- Resampling of residuals
boot_residuals <- replicate(B, {
  sim_data <- mtcars
  
  expected_mpg <- predict(base_model)
  
  # Sample with replacement from the empirical residuals
  resampled_noise <- sample(resid(base_model), size = nrow(sim_data), replace = TRUE)
  
  # Generate new response
  sim_data$mpg <- expected_mpg + resampled_noise
  
  coef(lm(mpg ~ wt + hp, data = sim_data))
})
funplot(boot_residuals)

# 3.- Resampling of cases
boot_cases <- replicate(B, {
  # Sample row indices with replacement
  resampled_rows <- sample(1:nrow(mtcars), size = nrow(mtcars), replace = TRUE)
  
  # Create the new dataframe
  resampled_data <- mtcars[resampled_rows, ]
  
  # Re-estimate the model on the new dataset
  coef(lm(mpg ~ wt + hp, data = resampled_data))
})
funplot(boot_cases)
boot_ci <- apply(boot_model_based, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.975))
t(boot_ci)

boot_ci <- apply(boot_residuals, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.975))
t(boot_ci)

boot_ci <- apply(boot_cases, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.975))
t(boot_ci)
