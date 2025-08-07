## Possion regression
#---------------------
library(dplyr)
library(car)
library(ggplot2)
library(gam)

# Data
data <- data.frame(
  visitors = c(42, 48, 39, 52, 44, 58, 51, 47, 41, 49, 40, 46, 43, 54, 50),
  day_of_week = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun",
                         "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon")),
  marketing_spend = c(200, 150, 180, 250, 220, 400, 350, 300, 160, 190,
                      140, 210, 380, 420, 170),
  temperature = c(22, 25, 19, 24, 26, 28, 30, 27, 21, 23, 18, 25, 29, 31, 20)
)

# check mean vs variance of Y
mean(data$visitors)
var(data$visitors)
var(data$visitors) / mean(data$visitors) # Should be close to 1

# Visualize the distribution of Y
ggplot(data, aes(x = visitors)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Daily Visitors",
       x = "Number of Visitors", y = "Frequency")

# fit model
model <- glm(visitors ~ day_of_week + marketing_spend + temperature,
             family = poisson, data = data)
summary(model)

# check assumptions
## method1
crPlots(model) 
## method2
gam_model <- gam(visitors ~ day_of_week + s(marketing_spend) + s(temperature), 
                 family = poisson(), data = data)
glm_model <- glm(visitors ~ day_of_week + marketing_spend + temperature, 
                 family = poisson(), data = data)
AIC(gam_model, glm_model)
anova(glm_model, gam_model, test = "Chisq")
## method3
data$log_fitted <- log(fitted(model))
ggplot(data, aes(x = marketing_spend, y = log_fitted)) +
  geom_point() +
  geom_smooth(method = "loess",se=F) +
  geom_smooth(method = "lm",col="red",se=F) +
  labs(title = "Linearity Check for x1", y = "log(fitted values)")

# Calculate Incidence Rate Ratios (IRRs)
exp(coefficients(model))

# Example with exposure data
data$exposure_days <- c(rep(7, 10), rep(6, 5)) 

# Model with offset
model_offset <- glm(visitors ~ day_of_week + marketing_spend + temperature + 
                      offset(log(exposure_days)), 
                    family = poisson, data = data)
summary(model_offset)

# Create new data for prediction
new_data <- data.frame(
  day_of_week = factor("Fri", levels = levels(data$day_of_week)),
  marketing_spend = 300,
  temperature = 25
)
# Predict expected counts
predicted_counts <- predict(model, newdata = new_data, type = "response")
print(paste("Expected visitors:", round(predicted_counts, 1)))

## check for overdispersion
residual_deviance <- model$deviance
df_residual <- model$df.residual
dispersion <- residual_deviance/df_residual
print(paste("Dispersion statistic:", round(dispersion, 3)))

if (dispersion > 1.5) {
  print("Possible overdispersion detected")
  print("Consider quasi-Poisson or negative binomial models")
}

## residual analysis
fitted_values <- fitted(model)
pearson_residuals <- residuals(model, type = "pearson")
plot(fitted_values, pearson_residuals,
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

# what to do in the case of over dispersion
# Fit quasi-Poisson model
quasi_model <- glm(visitors ~ day_of_week + marketing_spend + temperature, 
                   family = quasipoisson, data = data)
























