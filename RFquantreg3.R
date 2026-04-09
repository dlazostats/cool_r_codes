## Quantile Regression
#-----------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)
library(summarytools)
library(caret)
library(sjstats)
library(scoringRules)
library(ggplot2)

calculate_mad <- function(x) {
  median(abs(x - median(x)))
}
calculate_scaled_mad <- function(x) {
  1.4826 * median(abs(x - median(x)))
}
# set working directory
script_name <- 'RFquantreg3.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0[sample(1:nrow(db0),1500),] %>% dplyr::select(Fluencia,PesoMetrico:Pb,material_c)

# quick EDA
hist(db0_sqmple$Fluencia,breaks = "FD")
descr(db0_sqmple)

# Quantile Random Forest
set.seed(212)
indx<-createDataPartition(db0_sqmple$Fluencia,p=0.8,list=F)
train<-db0_sqmple[indx,]
test<-db0_sqmple[-indx,]
Xtrain<-train %>% dplyr::select(-Fluencia)
Ytrain<-train %>% dplyr::select(Fluencia) %>% pull()
Xtest<-test %>% dplyr::select(-Fluencia)
Ytest<-test %>% dplyr::select(Fluencia)%>% pull()

qrf <- quantregForest(x=Xtrain, y=Ytrain,keep.inbag=TRUE )
qrf
plot(qrf)
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
pred <- predict(qrf, newdata = Xtest, what = quantiles)
pred

# CDF is for one observations
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest[1, , drop=FALSE], what = fine_grid)
y_vals <- as.numeric(pred_full)
plot(y_vals, fine_grid, type = "l", lwd = 2,
     xlab = "y", ylab = "F(y | x)",
     main = "Conditional CDF — Observation 1")
abline(h = c(0.1, 0.5, 0.9), lty = 2, col = "gray60")  # reference quantiles

# CFD for multiple observations
pred_full <- predict(qrf, newdata = Xtest[1:5, ], what = fine_grid)
cdf_df <- lapply(1:5, function(i) {
  data.frame(
    obs = paste("Obs", i),
    y   = pred_full[i, ],
    cdf = fine_grid
  )
}) |> bind_rows()

ggplot(cdf_df, aes(x = y, y = cdf, color = obs)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = c(0.1, 0.5, 0.9), linetype = "dashed", color = "gray60") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Conditional CDF", x = "y", y = "F(y | x)", color = NULL) +
  theme_minimal()

q10 <- predict(qrf, newdata = Xtest, what = 0.10)
q90 <- predict(qrf, newdata = Xtest, what = 0.90)
uncertainty_df <- data.frame(
  obs         = 1:nrow(Xtest),
  uncertainty = q90 - q10
) |> cbind(Xtest)

# Plot uncertainty against each covariate to find what drives it
ggplot(uncertainty_df, aes(x = C, y = uncertainty)) +
  geom_point() +
  geom_smooth() +
  labs(title = "What drives prediction uncertainty?")

# compute probabilites of crossing a threshold
# P(Y > 500 | X) for each observation
prob_above <- predict(qrf, newdata = Xtest,
                      what = function(x) mean(x > 500))
# From your plot, Obs 3 would have a much higher P(Y > 520) than the others

# Average outcome in the worst 10% of cases
es <- predict(qrf, newdata = Xtest,
              what = function(x) mean(x[x <= quantile(x, 0.10)]))

#7. Model Comparison via CRPS (continuous ranked probability)
#-------------------------------------------------------------
# evaluates teh full predicted distribution against the true value - much richer than RMSE
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest, what = fine_grid)
crps_scores <- sapply(1:nrow(Xtest), function(i) {
  crps_sample(y = Ytest[i], dat = pred_full[i, ])
})
mean(crps_scores)  # lower = better distributional fit
#This lets you compare QRF against other probabilistic models fairly.

# PIT Histogram
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest, what = fine_grid)

# For each obs, find where the true y falls in the predicted CDF
pit_values <- sapply(1:nrow(Xtest), function(i) {
  # interpolate to get F(y_true | x)
  approx(x = pred_full[i, ], y = fine_grid, xout = Ytest[i])$y
})

hist(pit_values, breaks = 20, freq = FALSE,
     main = "PIT Histogram",
     xlab = "F(y_true | x)", ylab = "Density")
abline(h = 1, col = "red", lty = 2)  # uniform reference line

#Shape : Flat (uniform) Meaning:Well calibrated ✅
#Shape : U-shaped       Meaning:Intervals too narrow (overconfident)

# Quantile Coverage Plot (reliability diagram)
quantiles <- seq(0.05, 0.95, by = 0.05)
pred_q <- predict(qrf, newdata = Xtest, what = quantiles)

# Empirical coverage: how often does y_test fall below each quantile?
empirical_coverage <- sapply(1:length(quantiles), function(j) {
  mean(Ytest <= pred_q[, j])
})

plot(quantiles, empirical_coverage, type = "b", pch = 19,
     xlab = "Nominal quantile", ylab = "Empirical coverage",
     main = "Reliability Diagram", xlim = c(0,1), ylim = c(0,1))
abline(0, 1, col = "red", lty = 2)  # perfect calibration line

#3. Overlay Predicted Distribution vs Actual (per observation)
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest[1:6, ], what = fine_grid)

# Build CDF dataframe + mark true y
cdf_df <- lapply(1:6, function(i) {
  data.frame(obs = paste("Obs", i),
             y   = pred_full[i, ],
             cdf = fine_grid,
             y_true = Ytest[i])
}) |> bind_rows()

ggplot(cdf_df, aes(x = y, y = cdf)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(aes(xintercept = y_true), color = "red",
             linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~obs, scales = "free_x") +
  labs(title = "Predicted CDF vs True Value (red)",
       x = "y", y = "F(y | x)") +
  theme_minimal()
#The red line should look like it's randomly placed along the CDF across observations — i
#f it's always on the left or right, there's systematic bias.

# CRPS per Observation (scoring)
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest, what = fine_grid)
crps_scores <- sapply(1:nrow(Xtest), function(i) {
  crps_sample(y = Ytest[i], dat = pred_full[i, ])
})
ggplot(data.frame(crps = crps_scores), aes(x = crps)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(crps_scores), color = "red", linetype = "dashed") +
  labs(title = "CRPS Distribution",
       subtitle = paste("Mean CRPS:", round(mean(crps_scores), 3)),
       x = "CRPS", y = "Count") +
  theme_minimal()

# conditional forest
#--------------------
















































