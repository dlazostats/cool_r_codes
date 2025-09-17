# Resampling
#-------------
library(rsample)
library(dplyr)
library(psych)
library(earth)
library(lattice)
library(mice)
library(purrr)
library(MLmetrics)

script_name <- 'resampling.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
dt0<-read.table("auto-mpg.data")
names(dt0)<-c("mpg","cylinders","displacement","horsepower" ,"weight","acceleration","year","origin","name")
head(dt0)
pairs.panels(dt0)     

# Data imputation
mt_imp<- dt0 %>% 
         mice(m=5, method = "pmm",seed = 123)  
plot(mt_imp)
sapply(dt0,function(x) sum(is.na(x)))
stripplot(mt_imp, mpg, pch = 19, xlab = "Imputation number")
stripplot(mt_imp, horsepower, pch = 19, xlab = "Imputation number")
densityplot(dt1)
dt1 <- mt_imp %>%  
       complete() 

# EDA and basic models
mod_form<-as.formula(mpg~displacement+horsepower+weight)
mars_mdl<-earth(mod_form,data=dt1)
lmm<-lm(mod_form,data=dt1)
plot(dt1$mpg,type="l")
lines(predict(lmm),col="red")
lines(predict(mars_mdl),col="blue")
RMSE(predict(lmm),dt1$mpg)
RMSE(predict(mars_mdl),dt1$mpg)

mars_one<-earth(mpg~displacement,data=dt1)
ggplot(dt1, aes(displacement, mpg)) +
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_line(aes(y=predict(mars_one)),color="red")+
  theme_bw()

# Model assessment
#---------------------
# 10 repeats of 10 fold cv
set.seed(2121)
rp_fold <- vfold_cv(dt1, v = 10, repeats = 10)
rp_fold

# holdout resoults
cv_res<-function(folds,...){
  modl<-earth(...,data=analysis(folds))
  holdout<-assessment(folds) 
  res<-holdout %>% 
       mutate(.pred=predict(modl,newdata=holdout)) %>% 
       dplyr::select(mpg,.pred)
  metric<-RMSE(res$.pred,res$mpg)
  return(metric)
}
rp_fold$results<-map(rp_fold$splits,
                     cv_res,
                     mod_form)
res_rmse<-rp_fold$results %>% unlist()
histogram(res_rmse)



nadeau_bengio_var <- function(scores) {
  K <- length(scores)
  mean_score <- mean(scores)
  s2 <- sum((scores - mean_score)^2) / (K - 1)
  var_nb <- (1/K) * s2 + (1/(K - 1)) * s2
  return(var_nb)
}

nadeau_bengio_var(res_rmse)
var(res_rmse)






