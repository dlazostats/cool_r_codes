#Practice ML
#------------
library(dplyr)
library(caret)
library(tidymodels)
library(jtools)
library(pROC)
library(PRROC)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)
db2<-db0 %>% 
  mutate(caliente=as.integer(grepl("CALIENTE", Tipo))) %>% 
  select(caliente ,PE_MET,Temp_lam,C:material_c)

# split into train/test
set.seed(2121)
db2s<-db2 %>% 
splitt<-initial_split(db2,prop=0.8,strata=caliente)
train<-training(splitt)
test<-testing(splitt)
prop_df <- combined %>%
  count(set, caliente) %>%
  group_by(set) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
ggplot(prop_df, aes(x = factor(caliente), y = prop, fill = set)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.85) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Target distribution: train vs test",
       x = "caliente (target)", y = "proportion", fill = "set")

# Practicing Logistic regression
#=================================
# basic logistic regression
logm1 <- glm(caliente ~ ., data = train, family = binomial)
summ(logm1)
summ(logm1, exp = TRUE)
ggeffects::ggpredict(logm1, "C")
ggeffects::ggpredict(logm1, terms = "C [all]")
plot(ggeffects::ggpredict(logm1, terms = "C [all]"))
p <- predict(logm1, type = "response")
roc_obj <- roc(train$caliente, p) # AUC (threshold-independent, good first look)
auc(roc_obj)
pred <- ifelse(p > 0.20, 1, 0)  # Instead of 0.5, try a threshold near the base rate
table(Predicted = pred, Actual = train$caliente)  # or 0.80 depending on which class you care about

# predict
p_test <- predict(logm1, newdata = test, type = "response")
auc(roc(test$caliente, p_test))

# use weights
w <- ifelse(train$caliente == 0,
            1 / mean(train$caliente == 0),
            1 / mean(train$caliente == 1))
logm_w <- glm(caliente ~ ., data = train, family = binomial, weights = w)
p <- predict(logm_w, type = "response")
roc_obj <- roc(train$caliente, p) # AUC (threshold-independent, good first look)
auc(roc_obj)
p_w <- predict(logm_w, newdata = test, type = "response")
auc(roc(test$caliente, p_w))

pred_plain <- ifelse(p_test > 0.5, 1, 0)
pred_weighted <- ifelse(p_w > 0.5, 1, 0)
table(Predicted = pred_plain,    Actual = test$caliente)
table(Predicted = pred_weighted, Actual = test$caliente)

# Other metrics
#----------------
pred <- factor(ifelse(p_test > 0.5, 1, 0), levels = c(0,1))
actual <- factor(test$caliente, levels = c(0,1))

confusionMatrix(pred, actual, positive = "1") 

# PR-AUC: scores for positives and negatives, class of interest = 1 here
pr <- pr.curve(scores.class0 = p_test[test$caliente == 1],
               scores.class1 = p_test[test$caliente == 0],
               curve = TRUE)
pr$auc.integral

# log loss
y <- test$caliente
-mean(y * log(p_test) + (1 - y) * log(1 - p_test))
base <- mean(train$caliente)
-mean(y * log(base) + (1 - y) * log(1 - base))

pr0 <- pr.curve(scores.class0 = (1 - p_test)[test$caliente == 0],
                scores.class1 = (1 - p_test)[test$caliente == 1],
                curve = TRUE)
pr0$auc.integral

# for the minority class
#-------------------------
p0 <- 1 - p_test
actual0 <- factor(ifelse(test$caliente == 0, "0", "1"), levels = c("0","1"))
thresholds <- seq(0.10, 0.90, by = 0.05)
sweep <- data.frame(
  threshold   = thresholds,
  sensitivity = NA,  # recall on class 0
  specificity = NA,
  precision   = NA,  # PPV on class 0
  f1          = NA,
  bal_acc     = NA
)
for (i in seq_along(thresholds)) {
  th   <- thresholds[i]
  pred <- factor(ifelse(p0 >= th, "0", "1"), levels = c("0","1"))
  
  tp <- sum(pred == "0" & actual0 == "0")
  fp <- sum(pred == "0" & actual0 == "1")
  fn <- sum(pred == "1" & actual0 == "0")
  tn <- sum(pred == "1" & actual0 == "1")
  
  sens <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  prec <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
  f1   <- ifelse(is.na(prec) | (prec + sens) == 0, NA,
                 2 * prec * sens / (prec + sens))
  
  sweep$sensitivity[i] <- round(sens, 3)
  sweep$specificity[i] <- round(spec, 3)
  sweep$precision[i]   <- round(prec, 3)
  sweep$f1[i]          <- round(f1, 3)
  sweep$bal_acc[i]     <- round((sens + spec) / 2, 3)
}

print(sweep)









