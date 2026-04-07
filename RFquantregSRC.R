install.packages("randomForestSRC")
library(randomForestSRC)
set.seed(123)
n <- 300
x1 <- runif(n)
x2 <- rnorm(n)
y  <- 2 + 3*x1 - 1.5*x2 + rnorm(n, sd = 0.5)
data <- data.frame(y, x1, x2)
