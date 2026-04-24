set.seed(2143)
b<-10000
x<-rnorm(b)
sum(x>1.5)/b

f <- function(n=1e4){ x<-rnorm(n); list(mc=mean(x>1.5), exact=pnorm(1.5,lower.tail=FALSE)) }

l<-c(5,30,200,1000)
lapply(l,function(x) mean(rexp(5,1)))

par(mfrow=c(1,3));
for(n in c(5,30,200)){ 
  means <- replicate(5000, mean(rexp(n)))
  hist(means, main=paste("n=",n), prob=TRUE, col="lightblue") 
}

# simulation
lambda_true <- 3
estimates <- replicate(10000, mean(rpois(50, lambda_true)))
mean(estimates)        # should be ≈ 3
var(estimates)         # should be ≈ lambda/n = 3/50 = 0.06

#ratio of two sample variances
ratio_dist <- replicate(10000, {
  x <- rexp(30); y <- rexp(30)
  var(x) / var(y)
})
quantile(ratio_dist, c(0.025, 0.975))

# power analysis
#What sample size do you need to detect an effect of size d = 0.4 with 80% power in a two-sample t-test?
power_sim <- function(n, d = 0.4, B = 5000) {
  rejections <- replicate(B, {
    x <- rnorm(n); y <- rnorm(n, mean = d)
    t.test(x, y)$p.value < 0.05
  })
  mean(rejections)
}
power_sim(50)   # ≈ 0.60 — not enough
power_sim(100)  # ≈ 0.80 — there it is

