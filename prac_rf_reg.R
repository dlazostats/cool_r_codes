## RF regression
#-----------------------
library(dplyr)
library(summarytools)
library(caret)
library(ggplot2)
library(ranger)
library(vip)
library(e1071)
library(party)
#library(partykit)

# set working directory
script_name <- 'prac_rf_reg.R'
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

# RF model
#------------
model <- ranger(
  formula       = Fluencia ~ .,
  data          = train,
  num.trees     = 500,
  mtry          = floor((ncol(train)-1)/3),              # ~sqrt(10 predictors)
  min.node.size = 5,              # default for regression
  importance    = "permutation",     # to get variable importance
  seed          = 42
)
vip(model)

# tuning using caret
ctrl <- trainControl(
  method  = "cv",  
  number  = 10,    
  selectionFunction = "oneSE",
  verboseIter = TRUE     # print progress
)
tune_grid <- expand.grid(
  mtry            = c(3, 4, 5, 6),
  splitrule       = c("variance", "extratrees","maxstat"),
  min.node.size   = c(3, 5, 10)
)
set.seed(42)
model_caret <- train(Fluencia ~ .,
                     data      = train,
                     method    = "ranger",
                     trControl = ctrl,
                     tuneGrid  = tune_grid,
                     metric    = "RMSE",      
                     num.trees = 500,
                     importance = "permutation")
model_caret
plot(model_caret)
model_caret$bestTune
densityplot(model_caret)
vip(model_caret)

# apply non parametric test
preds     <- predict(model_caret, newdata = test)
residuals <- test$Fluencia - preds
hist(residuals,breaks="FD")

# are residuals normally distributed
shapiro_test <- shapiro.test(residuals)
cat("\n── Shapiro-Wilk Test (normality of residuals) ──\n")
print(shapiro_test)

# 6b. Wilcoxon Signed-Rank — Is the median residual = 0? (unbiased model?)
wilcox_test <- wilcox.test(residuals, mu = 0, alternative = "two.sided")
cat("\n── Wilcoxon Signed-Rank Test (median residual = 0?) ──\n")
print(wilcox_test)
# p > 0.05 → model is not systematically biased

# 6c. Spearman Correlation — Are predictions monotonically correlated with actuals?
spearman_test <- cor.test(test$Fluencia, preds, method = "spearman")
cat("\n── Spearman Correlation (predictions vs actuals) ──\n")
print(spearman_test)
# rho close to 1 → strong monotonic relationship

# 6d. Kolmogorov-Smirnov — Do predictions follow the same distribution as actuals?
ks_test <- ks.test(preds, test$Fluencia)
cat("\n── Kolmogorov-Smirnov Test (distribution of preds vs actuals) ──\n")
print(ks_test)
# p > 0.05 → distributions are similar (good sign)

# Check this visually
plot(test$Fluencia, preds,
     xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")

# Check skewness
hist(test$Fluencia, breaks = 30, main = "Distribution of Fluencia")
shapiro.test(test$Fluencia)

# If skewed → try log-transforming the target
train$Fluencia_log <- log1p(train$Fluencia)
test$Fluencia_log  <- log1p(test$Fluencia)
so 

# 6e. Kruskal-Wallis — Do errors differ across price quantile groups?
test$flue_group <- cut(test$Fluencia,
                        breaks = quantile(test$Fluencia, probs = c(0, 0.33, 0.66, 1)),
                        labels = c("Low", "Mid", "High"),
                        include.lowest = TRUE)
test$abs_error <- abs(residuals)
kruskal_test <- kruskal.test(abs_error ~ flue_group, data = test)
cat("\n── Kruskal-Wallis Test (error across price groups) ──\n")
print(kruskal_test)
# p < 0.05 → model performs differently across price ranges

# Predicted vs Actual
ggplot(data.frame(actual = test$Fluencia, predicted = preds), 
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual", x = "Actual Price", y = "Predicted Price")

# Error by price group (boxplot)
ggplot(test, aes(x = flue_group, y = abs_error, fill = flue_group)) +
  geom_boxplot() +
  labs(title = "Absolute Error by Price Group (Kruskal-Wallis)", 
       x = "Price Group", y = "Absolute Error")

featurePlot(x = test[,c(21,1,2,4)], 
            y = test$flue_group, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

featurePlot(x = test[,c(21,1,2,4)],  
            y = test$flue_group, 
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))

# fix skeness problem
# |skew| > 1.0  → high skew, log transform likely to help
# |skew| < 0.5  → fairly symmetric, log transform may not help much
skewness(train$Fluencia)

train$Fluencia_log <- log1p(train$Fluencia)  # log1p = log(x+1), safe if x has zeros
test$Fluencia_log  <- log1p(test$Fluencia)
ctrl <- trainControl(
  method  = "cv",  
  number  = 10,    
  selectionFunction = "oneSE",
  verboseIter = TRUE     # print progress
)
tune_grid <- expand.grid(
  mtry            = c(3, 4, 5, 6),
  splitrule       = c("variance", "extratrees","maxstat"),
  min.node.size   = c(3, 5, 10)
)
set.seed(42)
model_caret <- train(Fluencia_log ~ .,
                     data      = train[, !names(train) %in% "Fluencia"],
                     method    = "ranger",
                     trControl = ctrl,
                     tuneGrid  = tune_grid,
                     metric    = "RMSE",      
                     num.trees = 1000,
                     importance = "permutation")
model_caret
plot(model_caret)
model_caret$bestTune
densityplot(model_caret)
vip(model_caret)

# Predict and BACK-TRANSFORM
preds_log        <- predict(model_caret, newdata = test)
preds_original   <- expm1(preds_log)  

ks.test(preds_original, test$Fluencia)

ggplot() +
  geom_density(aes(x = test$Fluencia,  fill = "Actual"),            alpha = 0.5) +
  geom_density(aes(x = preds,          fill = "Pred (original)"),   alpha = 0.5) +
  geom_density(aes(x = preds_original, fill = "Pred (log-transformed)"), alpha = 0.5) +
  scale_fill_manual(values = c("steelblue", "tomato", "green3")) +
  labs(title = "Distribution Comparison", x = "Fluencia", fill = "")

# conditional Random Forest
#---------------------------
model_cforest <- party::cforest(
  Fluencia ~ .,
  data    = train,
  controls = cforest_unbiased(   # recommended control settings
    ntree  = 500,
    mtry   = floor((ncol(train)-1)/3)
  )
)
model_cforest
party::varimp(model_cforest, conditional = TRUE)       # works cleanly
oob_preds <- predict(model_cforest, OOB = TRUE,type = "response")
# OOB RMSE
rmse <- sqrt(mean((oob_preds - train$Fluencia)^2))
cat("OOB RMSE:", round(rmse, 3), "\n")

test_preds <- predict(model_cforest, newdata = test, type = "response")
rmse_tst <- sqrt(mean((test_preds - test$Fluencia)^2))
cat("Test RMSE:", round(rmse_tst, 3), "\n")



vimp <- party::varimp(model_cforest, conditional = TRUE)
dotchart(sort(vimp), main = "Conditional Variable Importance")
# or with ggplot
library(ggplot2)
vimp_df <- data.frame(variable = names(vimp), importance = vimp)
ggplot(vimp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() + coord_flip() + labs(title = "Conditional Variable Importance")


library(pdp)
partial(model_cforest, pred.var = "PesoMetrico", plot = TRUE)

# residual analysis
residuals_oob <- oob_preds - train$Fluencia

# residuals vs fitted
plot(oob_preds, residuals_oob, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# residual distribution
hist(residuals_oob, main = "OOB Residuals",breaks="FD")

plot(test$Fluencia, test_preds,
     xlab = "Actual", ylab = "Predicted",
     main = "Predicted vs Actual")
abline(0, 1, col = "red")  # perfect prediction line




















