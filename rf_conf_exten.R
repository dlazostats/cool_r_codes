library(ggplot2)
library(tidymodels)
library(car)
library(DescTools)
library(corrplot)
library(dplyr)
library(quantregForest)
library(psych)
data(diamonds)
head(diamonds)

# extract a sample
dim(diamonds)
diamonds_s<-diamonds %>% 
            sample_frac(0.1)

#EDA
hist(diamonds_s$price)
scatterplotMatrix(~price+x+y+z,
                  data = diamonds_s)
corrplot(cor(diamonds_s %>% dplyr::select(where(is.numeric)),use="complete.obs"))
ggcorr(diamonds_s %>% dplyr::select(where(is.numeric)) %>% dplyr::select(price,everything()),
       label=T)

# better other dataset
#-----------------------
red_wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
white_wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
wine_total <- rbind(red_wine, white_wine)

# --- 1. Three-way split: train / calibration / test ---
df_split<-initial_validation_split(wine_total,strata=quality)
train<-training(df_split)
calib<-validation(df_split)
test<-testing(df_split)

# Quantile Regression
X_train <- as.matrix(train[, -which(names(train) == "quality")])
y_train <- train$quality
qrf <- quantregForest(x = X_train, y = y_train, ntree = 500)

X_test <- as.matrix(test[, -which(names(test) == "quality")])
preds <- predict(qrf, newdata = X_test, what = c(0.025, 0.5, 0.975))
colnames(preds) <- c("lower", "median", "upper")
preds

# using our dataset
#-----------------------
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/codes R propios")
df0<-read.csv("dftrain.csv")
df1<-df0 %>% 
     dplyr::select(Fluencia,PesoMetrico:Pb,material_c) %>% 
     filter(Fluencia<590)

#EDA 
Desc(df1$Fluencia,plotit=T)
Desc(as.factor(df1$material_c), plotit=TRUE)  
df1 %>% 
  sample_frac(0.15) %>% 
  psych::pairs.panels()


# --- 1. Three-way split: train / calibration / test ---
df_split<-initial_validation_split(df1,strata=Fluencia)
train<-training(df_split)
calib<-validation(df_split)
test<-testing(df_split)

{
  train$set  <- "train"
  calib$set  <- "calib"
  test$set   <- "test"
  df_all <- bind_rows(train, calib, test)
  ggplot(df_all, aes(x = Fluencia, color = set, fill = set)) +
    geom_density(alpha = 0.2) +
    labs(title = "Density Comparison of FLUENCIA",
         x = "FLUENCIA",
         y = "Density") +
    theme_minimal()  
}

# Quantile Regression
X_train <- train %>% dplyr::select(-Fluencia)
y_train <- train$Fluencia
qrf <- quantregForest(x = X_train, y = y_train, ntree = 500)

X_test <- test %>% dplyr::select(-Fluencia)
preds <- predict(qrf, newdata = X_test, what = c(0.025, 0.5, 0.975))
colnames(preds) <- c("lower", "median", "upper")
dfpred<-preds %>% as.data.frame()
names(dfpred)<-c("liminf","med","limsup")
plot(dfpred$med,type="l")
lines(dfpred$liminf,col="red")
lines(dfpred$limsup,col="red")



