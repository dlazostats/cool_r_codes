library(np)
library(dplyr)
library(MLmetrics)

mob <- read.csv("http://www.stat.cmu.edu/~cshalizi/uADA/15/hw/01/mobility.csv")
sapply(mob,function(x) sum(is.na(x)))
mob1<-mob %>% filter(!is.na(Mobility))
sapply(mob1 %>% dplyr::select(Population,Seg_racial,Commute,Income,Gini),function(x) sum(is.na(x)))

mob.lm2 <- lm(Mobility ~ Population + Seg_racial + Commute + Income + Gini, data=mob1)
summary(mob.lm2)

cv_erro1<-cvFit(mob.lm2, data = mob1, K = 5, R = 100,
                y = mob1$Mobility) # rmspe (root mean squared prediction error)
cv_erro1
cv_erro1$reps
histogram(cv_erro1$reps)
hist(cv_erro1$reps,breaks="FD")

cv_erro2<-cvFit(mob.lm2, data = mob1, K = 5, R = 100,
                y = mob1$Mobility,cost=mape)
cv_erro2
cv_erro2$reps
histogram(cv_erro2$reps)
hist(cv_erro2$reps,breaks="Fd")

# Pick bandwidth by automated cross-validation first
mob.npbw <- npregbw(formula=formula(mob.lm2), data=mob1, tol=1e-2, ftol=1e-2)
mob.np <- npreg(mob.npbw, data=mob1)
predict(mob.np)

plot(mob1$Mobility,type="l")
lines(predict(mob.lm2),col="red") #linear
lines(predict(mob.np),col="blue") #kernel regression

RMSE(predict(mob.lm2),mob1$Mobility)
RMSE(predict(mob.np),mob1$Mobility)



