# Generalized Random Forest
#--------------------------
library(grf)
library(partykit)



# Conditional Tree
#=================
set.seed(2121)
## basic example: conditional inference forest for cars data
cf <- cforest(dist ~ speed, data = cars)

## prediction of fitted mean and visualization
nd <- data.frame(speed = 4:25)
nd$mean  <- predict(cf, newdata = nd, type = "response")

myquantile <- function(y, w) quantile(rep(y, w), probs = c(0.1, 0.5, 0.9))
p <- predict(cf, newdata = nd, type = "response", FUN = myquantile)
colnames(p) <- c("lower", "median", "upper")
nd <- cbind(nd, p)

par(mfrow=c(1,2))

plot(dist ~ speed, data = cars)
lines(mean ~ speed, data = nd)

plot(dist ~ speed, data = cars, type = "n")
with(nd, polygon(c(speed, rev(speed)), c(lower, rev(upper)),
                 col = "lightgray", border = "transparent"))
points(dist ~ speed, data = cars)
lines(mean ~ speed, data = nd, lwd = 1.5)
lines(median ~ speed, data = nd, lty = 2, lwd = 1.5)
legend("topleft", c("mean", "median", "10% - 90% quantile"),
       lwd = c(1.5, 1.5, 10), lty = c(1, 2, 1),
       col = c("black", "black", "lightgray"), bty = "n")

tree1 <- partykit::gettree(cf, k = 4)
plot(tree1)
