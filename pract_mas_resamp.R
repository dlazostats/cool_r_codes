# Practicar resample
#--------------------
library(dplyr)
library(rsample)
library(earth)
library(ggplot2)
library(purrr)
library(broom)

#load data
data(Boston, package = "MASS")

# Only one predictor
#----------------------
df_one0<-Boston |> dplyr::select(medv,lstat) |> filter(medv<50)

## lm
ggplot(df_one0,aes(x=lstat,y=medv))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()
se_lm<-summary(lm(medv~lstat,data=df_one0))$sigma
se_lm

## earth
mars_mdl<-earth(medv~.,data=df_one0)
ggplot(df_one0, aes(x = lstat, y = medv)) +
  geom_point(alpha = 0.6) +  
  geom_line(data = df_one0, aes(x = lstat, y = predict(mars_mdl,newdata=df_one0)), 
            color = "blue", size = 1.2) +
  theme_minimal()
se_earth<-sqrt(mars_mdl$rss/(nrow(df_one0)-1))
se_earth

set.seed(123) 
boston_boots <- bootstraps(Boston, times = 1000)





