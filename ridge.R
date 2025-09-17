# Ridge Regression
#------------------
library(dplyr)
library(ggplot2)
library(lattice)
library(MASS)
library(Hmisc)
library(psych)
library(jtools)
library(caret)

# set working directory
setwd("C:/Users/dlazo/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/Diego/cool_r_codes")

# load data
db_dib0<-read.csv("diabetes.csv")

# Eda
hist(db_dib0$prog,breaks="FD")
hist(db_dib0$prog,breaks=30)
ggplot(db_dib0,aes(x=prog))+
  geom_histogram(color="black",fill="white")
truehist(db_dib0$prog, nbins = 20, col = "lightgray")
histogram(~ prog, data = db_dib0)
pairs.panels(db_dib0 %>% dplyr::select(prog,everything(),-sex))

#processing
db_dib1<-db_dib0 %>% 
         mutate(prog=prog-mean(prog)) %>% 
         mutate(across(.cols = -prog, .fns = min_max))
  
# linear regression
lm_mdl1<-lm(prog~.,data=db_dib0)
summ(lm_mdl1)
  
lm<-train(prog~ -1+.,
          data=db_dib0,
          method="lm",
          preProcess = "scale")  
summ(lm$finalModel)  

m1<-db_dib0 %>% dplyr::select(-prog) %>% as.matrix()


X <- min_max(db_dib0[ , !(names(db_dib0) == "prog") ])
y <- db_dib0$prog - mean(db_dib0$prog)
l1<-lm(y~Xs)
summary(l1)

min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
Xs<-db_dib0 %>% dplyr::select(-prog)
Xs<-sapply(Xs,min_max)

X <- model.matrix(prog ~ ., db_dib0)[ , -1]  # Removes intercept column
y <- db_dib0$prog
ridge_model <- glmnet(X, y, alpha = 0.1)
summary(ridge_model)
coef(ridge_model,s=ridge_model$lambda[91])
