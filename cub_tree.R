## Cubist tree
#-----------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)
library(summarytools)
library(pre)
library(ranger)
library(Metrics)
library(Cubist)
library(caret)

# set working directory
script_name <- 'cub_tree.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# functions
results <- list()
eval_metrics <- function(true, pred) {
  data.frame(
    RMSE = rmse(true, pred),
    MAE  = mae(true, pred),
    R2   = 1 - sum((true - pred)^2) / sum((true - mean(true))^2)
  )
}

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0[sample(1:nrow(db0),1500),] %>% dplyr::select(Fluencia,PesoMetrico:Pb,material_c)

# train test set
indx<-createDataPartition(db0_sqmple$Fluencia,p=0.8,list=F)
train<-db0_sqmple[indx,]
test<-db0_sqmple[-indx,]
Xtrain<-train %>% select(-Fluencia)
Ytrain<-train %>% select(Fluencia) %>% pull()
Xtest<-test %>% select(-Fluencia)
Ytest<-test %>% select(Fluencia)%>% pull()

cubist_model <- cubist(x = Xtrain, y = Ytrain, committees = 10)
cubist_pred  <- predict(cubist_model, newdata = Xtest)
results$Cubist <- eval_metrics(Ytest, cubist_pred)

rf_model <- ranger( Fluencia ~ ., data = train,num.trees = 500,mtry = 4)
rf_pred <- predict(rf_model, data = test)$predictions
results$RandomForest <- eval_metrics(Ytest, rf_pred)

summary(cubist_model)
cubist::cubist_rules(cubist_model)
s <- capture.output(summary(cubist_model))
rule_lines <- s[grepl("^Rule", s) | grepl("^\\s+\\w", s)]
cat(rule_lines, sep = "\n")

subs1<-train %>% filter(C<=0.30112 & TempLam > 675)
lm1<-lm(Fluencia~C+PesoMetrico+TempLam+Cr,data=subs1)
RMSE(predict(lm1),subs1$Fluencia)
plot(subs1$Fluencia)
hist(subs1$Fluencia,breaks="FD")
hist(predict(lm1),breaks="FD")

df_plot <- data.frame(
  actual = subs1$Fluencia,
  predicted = predict(lm1)
)

ggplot(df_plot, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  theme_bw() +
  labs(title = "Predicted vs Actual (Test)",
       x = "Actual",
       y = "Predicted")

library(rules)
rule_df <- tidy(cubist_model)
rule_df
rule_df$estimate[[1]]
rule_df$statistic[[1]]

rule_7 <- rule_df$rule[7]
rule_7 <- rlang::parse_expr(rule_7)
rule_7
