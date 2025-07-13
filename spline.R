#Spline
#-------
library(splines)
birthrates= read.csv("birthrate2.csv", row.names = 1)
head(birthrates)
plot(birthrates, pch = 19, col = "darkorange")

#polynomial regresion
#--------------------
par(mfrow=c(1,2))
par(mar = c(2,3,2,0))
lmfit<-lm(Birthrate~poly(Year,3),data=birthrates)
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, col = "deepskyblue", lwd = 2)
title("degree = 3")

par(mar = c(2,3,2,0))
lmfit <- lm(Birthrate ~ poly(Year, 5), data = birthrates)
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, col = "deepskyblue", lwd = 2)
title("degree = 5")

# histogran regression by parts
par(mfrow=c(1,1))
par(mar = c(2,2,2,0))

mybasis = matrix(NA, nrow(birthrates), 8)
for (l in 1:8){
  mybasis[, l] = birthrates$Year*(birthrates$Year >= 1917 + l*10)
}

lmfit <- lm(birthrates$Birthrate ~ ., data = data.frame(mybasis))
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, type = "s", col = "deepskyblue", lwd = 2)
title("Histgram Regression")

# piecewise polynomials
#----------------------
par(mfrow=c(1,2))

myknots = c(1936, 1960, 1978)
bounds = c(1917, myknots, 2003)  

# piecewise constant
mybasis = cbind("x_1" = (birthrates$Year < myknots[1]), 
                "x_2" = (birthrates$Year >= myknots[1])*(birthrates$Year < myknots[2]), 
                "x_3" = (birthrates$Year >= myknots[2])*(birthrates$Year < myknots[3]),
                "x_4" = (birthrates$Year >= myknots[3]))

lmfit <- lm(birthrates$Birthrate ~ . -1, data = data.frame(mybasis))
par(mar = c(2,3,2,0))    
plot(birthrates, pch = 19, col = "darkorange")
abline(v = myknots, lty = 2)
title("Piecewise constant")

for (k in 1:4){
  points(c(bounds[k], bounds[k+1]), rep(lmfit$coefficients[k], 2), type = "l", 
         lty = 1, col = "deepskyblue", lwd = 4)  
}


# piecewise linear
mybasis = cbind("x_1" = (birthrates$Year < myknots[1]), 
                "x_2" = (birthrates$Year >= myknots[1])*(birthrates$Year < myknots[2]), 
                "x_3" = (birthrates$Year >= myknots[2])*(birthrates$Year < myknots[3]),
                "x_4" = (birthrates$Year >= myknots[3]),
                "x_11" = birthrates$Year*(birthrates$Year < myknots[1]), 
                "x_21" = birthrates$Year*(birthrates$Year >= myknots[1])*(birthrates$Year < myknots[2]), 
                "x_31" = birthrates$Year*(birthrates$Year >= myknots[2])*(birthrates$Year < myknots[3]),
                "x_41" = birthrates$Year*(birthrates$Year >= myknots[3]))

lmfit <- lm(birthrates$Birthrate ~ .-1, data = data.frame(mybasis))
par(mar = c(2,3,2,0))  
plot(birthrates, pch = 19, col = "darkorange")
abline(v = myknots, lty = 2)
title("Piecewise linear")

for (k in 1:4){
  points(c(bounds[k], bounds[k+1]), 
         lmfit$coefficients[k] + c(bounds[k], bounds[k+1])*lmfit$coefficients[k+4], 
         type = "l", lty = 1, col = "deepskyblue", lwd = 4)
}
  
# how to make them continuous functions
par(mfrow=c(1,1))
pos <- function(x) x*(x>0)
mybasis = cbind("int" = 1, "x_1" = birthrates$Year, 
                "x_2" = pos(birthrates$Year - myknots[1]), 
                "x_3" = pos(birthrates$Year - myknots[2]),
                "x_4" = pos(birthrates$Year - myknots[3]))

par(mar = c(2,2,2,0))
matplot(birthrates$Year, mybasis[, -1], type = "l", lty = 1, 
        yaxt = 'n', ylim = c(0, 50), lwd = 2)
title("Spline Basis Functions")

lmfit <- lm(birthrates$Birthrate ~ .-1, data = data.frame(mybasis))
par(mar = c(2,3,2,0))  
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, col = "deepskyblue", lwd = 4)
abline(v = myknots, lty = 2)
title("Linear Spline")

# using splines
par(mfrow=c(1,2))
par(mar = c(2,2,2,0))
lmfit <- lm(Birthrate ~ splines::bs(Year, degree = 3, knots = myknots), data = birthrates)
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, col = "deepskyblue", lwd = 4)
title("Cubic spline with 3 knots")

par(mar = c(2,2,2,0))
lmfit <- lm(Birthrate ~ splines::bs(Year, degree = 2, knots = myknots), data = birthrates)
plot(birthrates, pch = 19, col = "darkorange")
lines(birthrates$Year, lmfit$fitted.values, lty = 1, col = "deepskyblue", lwd = 4)
title("Cuadratic spline with 3 knots")

# spline Basis
## Natural Splines
par(mfrow=c(1,1))
fit.bs = lm(Birthrate ~ bs(Year, df=6), data=birthrates)
plot(birthrates$Year, birthrates$Birthrate, ylim=c(0,280), pch = 19, 
     xlim = c(1900, 2020), xlab = "year", ylab = "rate", col = "darkorange")   
lines(seq(1900, 2020), predict(fit.bs, data.frame("Year"= seq(1900, 2020))),
      col="deepskyblue", lty=1, lwd = 3)    
## Warning in bs(Year, degree = 3L, knots = c(1938.5, 1960, 1981.5), Boundary.knots = c(1917L, : some
## 'x' values beyond boundary knots may cause ill-conditioned bases

fit.ns = lm(Birthrate ~ ns(Year, df=6), data=birthrates)    
lines(seq(1900, 2020), predict(fit.ns, data.frame("Year"= seq(1900, 2020))), 
      col="darkgreen", lty=1, lwd = 3)
legend("topright", c("Cubic B-Spline", "Natural Cubic Spline"), 
       col = c("deepskyblue", "darkgreen"), lty = 1, lwd = 3, cex = 1.2)
title("Birth rate extrapolation")

#smoothing spline
par(mfrow=c(1,2))
fit = smooth.spline(birthrates$Year, birthrates$Birthrate)
plot(birthrates$Year, birthrates$Birthrate, pch = 19, 
     xlab = "Year", ylab = "BirthRates", col = "darkorange")
lines(seq(1917, 2003), predict(fit, seq(1917, 2003))$y, col="deepskyblue", lty=1, lwd = 3)

fit = smooth.spline(birthrates$Year, birthrates$Birthrate,cv=T)
plot(birthrates$Year, birthrates$Birthrate, pch = 19, 
     xlab = "Year", ylab = "BirthRates", col = "darkorange")
lines(seq(1917, 2003), predict(fit, seq(1917, 2003))$y, col="deepskyblue", lty=1, lwd = 3)

par(mfrow=c(1,1))
set.seed(1)
n = 100
x = seq(0, 1, length.out = n)
y = sin(12*(x+0.2))/(x+0.2) + rnorm(n)

# fit smoothing spline
fit = smooth.spline(x, y)

# the degrees of freedom
fit$df    

# fitted model
plot(x, y, pch = 19, xlim = c(0, 1), xlab = "x", ylab = "y", col = "darkorange")
lines(x, sin(12*(x+0.2))/(x+0.2), col="red", lty=1, lwd = 3)    
lines(x, predict(fit, x)$y, col="deepskyblue", lty=1, lwd = 3)
legend("bottomright", c("Truth", "Smoothing Splines"), 
       col = c("red", "deepskyblue"), lty = 1, lwd = 3, cex = 1.2)

#Extending Splines to Multiple Varibles
library(ElemStatLearn)
library(gam)

form = formula("chd ~ ns(sbp,df=4) + ns(tobacco,df=4) + 
                          ns(ldl,df=4) + famhist + ns(obesity,df=4) + 
                          ns(alcohol,df=4) + ns(age,df=4)")
m = gam(form, data=SAheart, family=binomial)
summary(m)
plot(m)
par(mfrow = c(3, 3), mar = c(5, 5, 2, 0))
plot(m, se = TRUE, residuals = TRUE, pch = 19, col = "darkorange")
  