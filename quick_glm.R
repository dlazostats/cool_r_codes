# Quick GLM
#-----------
library(MASS)
library(jtools)
library(ggplot2)
library(ggfortify)
library(lindia)

# fit logistic regression
head(Pima.tr)
glm_log<-glm(type~npreg+glu+bmi,
             family=binomial(link='logit'),
             data=Pima.tr)
summary(glm_log)
summ(glm_log)

# homogeneity of variance
head(mtcars)
lm<-lm(mpg~disp+hp+drat,data=mtcars)
summary(lm)
rlm1<-lm$residuals
fitted<-predict(lm)
ggplot(mtcars,aes(x=fitted,y=rlm1))+
  geom_point(col='blue')+ geom_abline(slope = 0)
autoplot(lm)
gg_diagnose(lm)
bptest(lm)

#glm
glm_lm<-glm(mpg~disp+hp+drat,
            family=gaussian(),
            data=mtcars)
summary(glm_lm)
bptest(glm_lm)

library(lmtest)
bptest(lm)
