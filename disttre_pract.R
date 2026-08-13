#Practice ML
#------------
library(dplyr)
library(rpart)
library(disttree)
library(tidymodels)
library(MLmetrics)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c) %>% slice_sample(n=1000)
head(db1);dim(db1)

# split into train/test
set.seed(2121)
splitt<-initial_split(db1,prop=0.8,strata=RES_TRA)
train<-training(splitt)
test<-testing(splitt)
combined <- bind_rows(
  train %>% mutate(set = "train"),
  test  %>% mutate(set = "test")
)
ggplot(combined, aes(x = RES_TRA, fill = set, color = set)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  theme_minimal() +
  labs(title = "Target density: train vs test", x = "target")


# disttree
#----------
dist_model <- disttree(RES_TRA ~ ., 
                       data = train, 
                       family =NO())
dist_model
plot(dist_model)

# disforest
# Fit a distributional random forest
forest_model <- distforest(RES_TRA ~ ., 
                           data = train, 
                           family = NO)
summary(forest_model)

pred_params <- predict(forest_model, newdata = test, type = "parameter") %>% 
               mutate(real=test$RES_TRA) %>% 
               mutate(lim_sup=mu+2*sigma,
                      lim_inf=mu-2*sigma) %>% mutate(width=lim_sup-lim_inf)
pred_params
plot(pred_params$real,type="l")
lines(pred_params$mu,col="red")
lines(pred_params$lim_sup,col="blue")
lines(pred_params$lim_inf,col="blue")
pred_10 <- qnorm(p = 0.025, 
                 mean = pred_params$mu, 
                 sd = pred_params$sigma)

pred_90 <- qnorm(p = 0.975, 
                 mean = pred_params$mu, 
                 sd = pred_params$sigma)

pred_paramsf<-pred_params %>% 
              mutate(lim_supq=pred_90,
                     lim_infq=pred_10)

RMSE(pred_params$mu,pred_params$real)




