library(ggplot2)
library(tidyr)
library(DataExplorer)

n = 100
z = rnorm(n, mean=0, sd=1) 
x = seq(-3,3,length=100)
ecdf.fun = ecdf(z) # Create the ECDF
class(ecdf.fun) 
ecdf.fun(0)

# We can plot it 
plot(x, ecdf.fun(x), lwd=2, col="red", type="l", ylab="CDF", main="ECDF")
lines(x, pnorm(x), lwd=2)
legend("topleft", legend=c("Empirical", "Actual"), lwd=2, 
       col=c("red","black"))

# B is a gaussian process b(0)=b(1)=0
n = 500
t = 1:n/n
Sig = t %o% (1-t)
Sig = pmin(Sig, t(Sig))
eig = eigen(Sig)
Sig.half = eig$vec %*% diag(sqrt(eig$val)) %*% t(eig$vec)
B = Sig.half %*% rnorm(n)
plot(t, B, type="l")

#Kolmogorov-Smirnov test
# It is distribution-free, meaning that the null distribution doesn’t depend on F,G
# We can actually compute the null distribution and use this test, e.g., via ks.test()
# Compares the entire distributions of two samples to see if they could plausibly come from the same underlying distribution.
ks.test(rnorm(n), rt(n, df=1)) # Normal versus t1
ks.test(rnorm(n), rt(n, df=10))

# histogram
hist.obj = hist(z, breaks=30, plot=FALSE) 
z
hist.obj$breaks
hist.obj$density

# We can plot it
plot(hist.obj, col="pink", freq=FALSE, main="Histogram")
lines(x, dnorm(x), lwd=2)
legend("topleft", legend=c("Histogram", "Actual"), lwd=2, 
       col=c("pink","black"))

# Example
# Simulate, supposing 60 subjects in each group 
set.seed(0)
n = 60 
mu.drug = 2
mu.nodrug = runif(n, min=0, max=1)
x.drug = 100*rexp(n, rate=1/mu.drug) 
x.nodrug = 100*rexp(n, rate=1/mu.nodrug)

# Find the range of all the measurements together, and define breaks
x.range = range(c(x.nodrug,x.drug))
breaks = seq(min(x.range),max(x.range),length=20)

# Produce hist of the non drug measurements, then drug measurements on top
hist(x.nodrug, breaks=breaks, probability=TRUE, xlim=x.range, 
     col="lightgray", xlab="Percentage reduction in tumor size", 
     main="Comparison of tumor reduction")

# Plot a histogram of the drug measurements, on top
hist(x.drug, breaks=breaks, probability=TRUE, col=rgb(1,0,0,0.2), add=TRUE) 

# Draw estimated densities on top, for each dist
lines(density(x.nodrug), lwd=3, col=1)
lines(density(x.drug), lwd=3, col=2)
legend("topright", legend=c("No drug","Drug"), lty=1, lwd=3, col=1:2)

## Basic Modelling
pros.df = read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat")
dim(pros.df)

pros.df |>
  pivot_longer(where(is.numeric)) |>
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

pairs(~ lpsa + lcavol + lweight + lcp, data=pros.df)

pros.df$svi = factor(pros.df$svi) 
par(mfrow=c(1,2))
plot(pros.df$svi, pros.df$lcavol, main="lcavol versus svi",
     xlab="SVI (0=no, 1=yes)", ylab="Log cancer volume")
plot(pros.df$svi, pros.df$lweight, main="lweight versus svi",
     xlab="SVI (0=no, 1=yes)", ylab="Log cancer weight")

library(gam, quiet=TRUE)
pros.gam = gam(lpsa ~ s(lcavol) + lweight, data=pros.df)

plot(pros.gam)
















