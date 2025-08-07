## bootstraping
#----------------
library(boot)
library(latticeExtra)
library(ggplot2)

cor(mtcars)
cor_fun<-function(df,index){
  cor(df[index,1],df[index,2])
}
bt1<-boot(mtcars,cor_fun,R=2000)
bt1
cor(mtcars[,1],mtcars[,2])
boot.ci(bt1)
bt1$t0
dfbt <- data.frame(s=bt1$t)
btci_rs<-boot.ci(bt1)
cor_or <- cor(mtcars[,1],mtcars[,2]) 

ggplot(dfbt, aes(x = s)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  labs(
    title = "ECDF of Bootstrap Resample Means",
    x = "Bootstrap Mean",
    y = "Empirical Cumulative Probability"
  ) +
  theme_minimal()

ggplot(dfbt, aes(x = s)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = cor_or, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Bootstrap Results: Histogram + Density",
       x = "Bootstrap Estimates",
       y = "Density") +
  theme_minimal()

ggplot(dfbt, aes(x = s)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = cor_or, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = btci_rs$bca[[4]], color = "grey", linetype = "dashed", size = 1) +
  geom_vline(xintercept = btci_rs$bca[[5]], color = "grey", linetype = "dashed", size = 1) +
  labs(title = "Bootstrap Results: Histogram + Density+BCa confidence interval",
       x = "Bootstrap Estimates",
       y = "Density") +
  theme_minimal()

jack.after.boot(bt1, useJ = FALSE, stinf = FALSE)
jack.after.boot(bt1)
plot(bt1)

# Jack after boot
library(boot)
library(bootstrap)
set.seed(1111)
theta.boot <- function(patch, i) {
  # function to compute the patch ratio statistic
  y <- patch[i, "y"]
  z <- patch[i, "z"]
  mean(y) / mean(z)
}
boot.out <- boot(bootstrap::patch,
                 statistic = theta.boot, R=2000)

# jackknife-after-bootstrap to est. se(se)
#------------------------------------------
A <- boot.array(boot.out)
theta.b <- boot.out$t
n <- NROW(patch)
jack.se <- numeric(n)
for (i in 1:n) {
  #in i-th replicate omit all samples with x[i]
  keep <- which(A[, i] == 0)
  jack.se[i] <- sd(theta.b[keep])
}
print(boot.out) #for se_boot
se.bar <- mean(jack.se)
se.se <- sqrt((n-1) * mean((jack.se - se.bar)^2))
print(paste("Jackknife-after-bootstrap est. se(se)=", se.se))

# Example
# initialize
data(patch, package = "bootstrap")
y <- patch$y
z <- patch$z
dat <- cbind(y, z)
n <- NROW(dat)
B <- 2000

#step 1: run the bootstrap
theta_boot <- function(dat, ind) {
  # function to compute the statistic
  y <- dat[ind, 1]
  z <- dat[ind, 2]
  mean(y) / mean(z)
}

boot.obj <- boot(dat, statistic = theta_boot, R=2000)
theta.hat <- boot.obj$t0
theta.b <- boot.obj$t
se.boot <- sd(theta.b) # sd of the bootstrap resamples
bias <- mean(boot.obj$t) - boot.obj$t0
abs(bias)/se.boot
boot.ci(boot.obj)

#jackknife-after-bootstrap to est. se(se)
sample.freq <- boot.array(boot.obj)
se.se.reps <- numeric(n)
N <- 1:n
for (i in N) {
  # jackknife-after-bootstrap
  # omit all bootstrap samples that contain obs i
  keep <- which(sample.freq[, i] == 0)
  se.se.reps[i] <- sd(theta.b[keep])
}
print(boot.obj)
se.bar <- mean(se.se.reps)
se.se <- sqrt((n-1) * mean((se.se.reps - se.bar)^2))
se.se

MASS::truehist(theta.b)

# Regression
library(ggplot2)
library(DAAG)

L1 <- lm(magnetic ~ chemical, data=ironslag)
cf3 <- round(L1$coeff, 3)
cap <- paste("Fit: magnetic =", cf3[1], "+", cf3[2], "chemical")


























