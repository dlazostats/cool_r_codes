#MC for inference
#-----------------
# a) basic MC estimation : difference between two normal random variables
m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}
est <- mean(g)
sd(g)/sqrt(m)
sqrt(sum((g-mean(g))^2))/m

# confidence interval
n <- 20
alpha <- .05
x <- rnorm(n, mean=0, sd=2)
UCL <- (n-1)*var(x)/qchisq(alpha, df=n-1)

n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
  x <- rnorm(n, mean = 0, sd = 2)
  (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
sum(UCL > 4)
mean(UCL > 4)


