library(bootstrap)
library(boot)
library(dplyr)
library(MLmetrics)
data(tooth)
tooth
m_d<-lm(strength~D1+D2,data=tooth)
summary(m_d)

plot(tooth$strength,type="l")
lines(predict(m_d),col="red")


m_e<-lm(strength~E1+E2,data=tooth)
summary(m_e)
plot(tooth$strength,type="l")
lines(predict(m_e),col="red")

# metrics
dp_tooth<-tooth %>%
          mutate(pred_d=predict(m_d),
                 pred_e=predict(m_e)) %>% 
          mutate(mse_d=c(strength-pred_d)^2,
                 mse_e=c(strength-pred_e)^2)
rse_d=sum(dp_tooth$mse_d)
rse_e=sum(dp_tooth$mse_e)
thts<-(1/nrow(tooth))*(rse_e-rse_d)
plot(dp_tooth$pred_d,dp_tooth$pred_e)
abline(a = 0, b = 1, col = "red", lwd = 2)

(rse_e-rse_d)/(rse_e+rse_d)

# fun_rse
fun_sse_diff <- function(data, indx) {
  dt  <- data[indx, ]
  m_d <- lm(strength ~ D1 + D2, data = dt)
  m_e <- lm(strength ~ E1 + E2, data = dt)
  sqerr_d <- (dt$strength - predict(m_d))^2
  sqerr_e <- (dt$strength - predict(m_e))^2
  mean(sqerr_e - sqerr_d)   # >0 ⇒ D fits better
}
botres <- boot(tooth, fun_sse_diff, R = 2000)
botres
hist(botres$t, breaks = "FD")

