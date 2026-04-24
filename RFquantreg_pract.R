# library
library(quantregForest)
library(caret)
library(ggplot2)

# set working directory
script_name <- 'RFquantreg_pract.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0 %>% 
            slice_sample(n = 1500)%>% 
            dplyr::select(Fluencia,PesoMetrico:Pb,material_c)

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
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
pred <- predict(qrf, newdata = Xtest, what = quantiles)
pred

df <- as.data.frame(pred)
colnames(df) <- paste0("q", quantiles)
df$idx <- order(df$q0.5)  # sort by median
df <- df[order(df$q0.5), ]
df$x <- 1:nrow(df)

ggplot(df, aes(x = x)) +
  geom_ribbon(aes(ymin = q0.1,  ymax = q0.9),  fill = "lightblue", alpha = 0.4) +
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = q0.5), color = "black", linewidth = 0.8) +
  labs(x = "Observation (sorted by median)", y = "Predicted value",
       title = "Quantile prediction intervals") +
  theme_minimal()

df <- data.frame(
  actual   = Ytest,
  median   = pred[, 3],
  lower    = pred[, 1],
  upper    = pred[, 5]
)

ggplot(df, aes(x = actual, y = median)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), color = "lightblue", alpha = 0.5) +
  geom_point(size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual", y = "Predicted median",
       title = "Predicted vs Actual with 80% interval") +
  theme_minimal()

mean(Ytest >= pred[,1] & Ytest <= pred[,5])  # should be ~0.80

## estimate conditional standard deviation and conditional mean (as in original RF)
conditionalSd <- predict(qrf,  Xtest, what=sd)
conditionalMean <- predict(qrf,  Xtest, what=mean)

results <- data.frame(
  mean = conditionalMean,
  sd   = conditionalSd,
  cv   = conditionalSd / conditionalMean,  # coefficient of variation
  lower_95 = conditionalMean - 1.96 * conditionalSd,
  upper_95 = conditionalMean + 1.96 * conditionalSd
)

# Flag observations where model is least confident
high_uncertainty <- which(conditionalSd > quantile(conditionalSd, 0.9))
high_uncertainty

# compare mean vs median
plot(conditionalMean, pred[, 3],  # pred[,3] = median from earlier
     xlab = "Conditional mean", ylab = "Conditional median",
     main = "Mean vs Median — symmetry check")
abline(0, 1, col = "red", lty = 2)

# uncertainty across observations
df <- data.frame(
  idx  = 1:length(conditionalMean),
  mean = conditionalMean,
  sd   = conditionalSd
)

ggplot(df[order(df$mean), ] |> transform(x = 1:nrow(df)), aes(x = x)) +
  geom_ribbon(aes(ymin = mean - 1.96*sd, ymax = mean + 1.96*sd), 
              fill = "steelblue", alpha = 0.3) +
  geom_line(aes(y = mean), color = "black") +
  labs(title = "Conditional mean ± 1.96 SD", x = "Obs (sorted by mean)", y = "Value") +
  theme_minimal()

# Combine with quantiles for a richer picture
df$q10 <- pred[, 1]
df$q90 <- pred[, 5]
df$interval_width <- df$q90 - df$q10

# Does SD track interval width? (sanity check)
plot(conditionalSd, df$interval_width,
     xlab = "Conditional SD", ylab = "q90 - q10 width",
     main = "SD vs quantile interval width")

# CDF is for one observations
fine_grid <- seq(0.01, 0.99, by = 0.01)
pred_full <- predict(qrf, newdata = Xtest[1, , drop=FALSE], what = fine_grid)
y_vals <- as.numeric(pred_full)
plot(y_vals, fine_grid, type = "l", lwd = 2,
     xlab = "y", ylab = "F(y | x)",
     main = "Conditional CDF — Observation 1")
abline(h = c(0.1, 0.5, 0.9), lty = 2, col = "gray60")  # reference quantiles

# See the actual weighted distribution
pred_dist <- predict(qrf, newdata = Xtest[1, , drop=FALSE], 
                     what = function(x) x)  # returns raw leaf values
hist(pred_dist, breaks = 30, main = "Weighted leaf values — Obs 1",
     xlab = "y", col = "steelblue")

# Check empirical coverage — should be ~0.80
coverage <- mean(Ytest >= pred[, 2] & Ytest <= pred[, 4])  
# pred[,2] = Q10, pred[,4] = Q90 from your earlier quantiles object
cat("Empirical 80% coverage:", round(coverage, 3))
#If this returns ~0.80, your intervals are well-calibrated and the probabilistic statement is 
# trustworthy. If it returns 0.65 or 0.95, the model is systematically over- or under-confident.





























