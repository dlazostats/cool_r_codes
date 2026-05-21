library(cmna)
library(pracma)

# interpolacion y extraplacion
x <- c(-3,-1, 3, 1)
y <- c(1- tan(1), 1- tan(1/2), 1 + tan(1/2), 1)
plot(x,y)
p <- polyinterp(x, y)
plot(x,y)


x <- c(-3,-1, 3, 1)
y <- c(1- tan(1), 1- tan(1/2), 1 + tan(1/2), 1)
p <- polyinterp(x, y)
plot(x,y)


xx <- seq(min(x), max(x), length.out = 200)
yy <- polyval(p, xx)
plot(x, y, pch = 19)
lines(xx, yy, col = "blue", lwd = 2)

## Differentiation
#------------------
findiff <- function(f, x, h = x*sqrt(.Machine$double.eps)) {
  return((f(x + h)- f(x)) / h)
}
f <- function(x) { 3*x- 1 }
findiff(f, 4, h = 1)



