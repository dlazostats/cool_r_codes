library(forecast)
library(tsfknn)

dta_ar0<-read.delim("clipboard")
#sel data
dta_ar1<-dta_ar0[,c(1,2)] %>% mutate(Familia=as.Date(paste0(Familia,"-01")))
names(dta_ar1)<-c("date","y")
plot(dta_ar1$y,type="l")

#fit model
dta_ar0 <- data.frame(
  date = seq(as.Date("2024-03-01"), by = "month", length.out = 24),
  y = c(16,71,81,32,0,0,48,110,229,76,88,68,
        45,49,109,16,77,98,89,113,94,85,66,70)
)
ts_data <- ts(dta_ar1$y, start = c(2024, 3), frequency = 12)
model <- arima(ts_data, order = c(5, 1, 1))
summary(model)
fc <- forecast(model, h = 6)
plot(fc)

# auto arima
modelaa<-auto.arima(ts_data)
fc_auto <- forecast(modelaa, h = 6)
plot(fc_auto)

# compare models
m1 <- arima(ts_data, order = c(5,1,1))
m2 <- auto.arima(ts_data,ic="bic")

data.frame(models=c("auto","man"),AIC=c(AIC(m1),AIC(m2)), BIC=c(BIC(m1),BIC(m2)))

acf(residuals(m1), main="ACF Residuals ARIMA(5,1,1)")
acf(residuals(m2), main="ACF Residuals auto.arima")

Box.test(residuals(m1), lag = 10, type = "Ljung-Box")
Box.test(residuals(m2), lag = 10, type = "Ljung-Box")

checkresiduals(m1)
checkresiduals(m2)           

accuracy(m1)
accuracy(m2)

#test
train <- window(ts_data, end = c(2025, 8))
test  <- window(ts_data, start = c(2025, 9))

m1 <- arima(train, order = c(5,1,1))
m2 <- auto.arima(train)

fc1 <- forecast(m1, h = length(test))
fc2 <- forecast(m2, h = length(test))

accuracy(fc1, test)
accuracy(fc2, test)

# croston
croston_model <- croston(ts_data)
croston_model
plot(croston_model)

croston_sba <- croston(ts_data, type = "sba")
croston_sba
plot(croston_sba)

croston_tsb <- croston(ts_data, type = "sbj")
croston_tsb
plot(croston_tsb)

library(tsintermittent)

# SBA
fit_sba <- crost(ts_data, type = "sba")

# TSB
fit_tsb <- tsb(ts_data)

# Automatic selection
fit_auto <- idclass(ts_data)
fit <- tsf_knn(ts_data, k = 3, lags = 1:6)

fc <- predict(fit, h = 6)

