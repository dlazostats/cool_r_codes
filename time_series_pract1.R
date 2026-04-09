# time series analysis
#----------------------
library(tseries)

# Simulate a non-stationary series (random walk)
set.seed(42)
rw <- cumsum(rnorm(200))
adf.test(rw)  # expect p-value >> 0.05

# After differencing to remove trend
adf.test(diff(rw))  

plot(rw,type="l")
lines(diff(rw),col="red")

# Simulate a stationary series
st <- rnorm(200)
adf.test(st) # expect p-value < 0.05

# With real data — e.g., AirPassengers dataset
adf.test(AirPassengers)  # stacionary

library(urca)
# "trend" includes constant + trend, "drift" includes only constant, "none" includes neither
result <- ur.df(rw, type = "trend", selectlags = "AIC")
summary(result)

# example
# Use log of AirPassengers (common practice to stabilize variance)
ap <- log(AirPassengers)
test_level <- ur.df(ap, type = "trend", selectlags = "AIC")
summary(test_level)
test_diff <- ur.df(diff(ap), type = "drift", selectlags = "AIC")
summary(test_diff)

plot(ap, main = "Log AirPassengers")  # clear upward trend → use "trend"
plot(diff(ap), main = "Differenced") 
