#Bootstrapping local regression line chart
#=========================================
library(boot)
plot(cars$speed, cars$dist, pch = 20, col = "black", xlab = "Car speed in mph",
     ylab = "Stopping distance in ft", main = "Speed and stopping distances of cars")
lines(cars$speed, loess(dist ~ speed, cars)$fitted, lwd = 3,
      col = "tomato")

# boot function
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  d <- d[order(d$speed),]
  loess_fit <- loess(dist ~ speed, d,
                     control = loess.control(surface = "direct"))
  predict(loess_fit, data.frame(speed = seq(4, 25, 1)), se = T)$fit
}
loess_boot <- boot(cars, R = 100, statistic = boot_fn)

#Plot
plot(cars$speed, cars$dist, pch = 20, col = "black", xlab = "Car speed in mph",
     ylab = "Stopping distance in ft", main = "Speed and stopping distances of cars")

for(i in sample(nrow(loess_boot$t), 20)) {
  lines(seq(4, 25, 1), loess_boot$t[i,], col = "gray")
}
lines(cars$speed, loess(dist ~ speed, cars)$fitted, lwd = 3,
      col = "tomato")


# confidence intervals
plot(cars$speed, cars$dist, pch = 20, col = "black", xlab = "Car speed in mph",
     ylab = "Stopping distance in ft", main = "Speed and stopping distances of cars")

conf_97.5 <- apply(loess_boot$t, 2, function(x) quantile(x, .975))
conf_2.5 <- apply(loess_boot$t, 2, function(x) quantile(x, .025))
lines(seq(4, 25, 1), conf_97.5, type ="l",
      col = "gray")
lines(seq(4, 25, 1), conf_2.5, type ="l",
      col = "gray")

lines(cars$speed, loess(dist ~ speed, cars)$fitted, lwd = 3,
      col = "tomato")


# practicing
#-------------
data("bodyfat", package = "TH.data")
loess_fit <- loess(DEXfat ~ age, data = bodyfat)
sorted_age <- sort(bodyfat$age)
sorted_fit <- predict(loess_fit, newdata = data.frame(age = sorted_age))
plot(bodyfat$age,bodyfat$DEXfat,pch = 20, col = "black")
lines(sorted_age, sorted_fit,lwd = 3,col="red")

# boot function
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  d <- d[order(d$age),]
  loess_fit <- loess(DEXfat ~ age, d,
                     control = loess.control(surface = "direct"))
  sorted_age <- sort(data$age)
  predict(loess_fit, data.frame(age = sorted_age), se = T)$fit
}
loess_boot <- boot(bodyfat, R = 100, statistic = boot_fn)

plot(bodyfat$age, bodyfat$DEXfat, pch = 20, col = "black", xlab = "Car speed in mph",
     ylab = "Stopping distance in ft", main = "Speed and stopping distances of cars")

for(i in sample(nrow(loess_boot$t), 20)) {
  lines(sorted_age, loess_boot$t[i,], col = "gray")
}
lines(sorted_age, sorted_fit,lwd = 3,col="red")


