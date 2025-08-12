#Jackknife-after-bootstrap
#==========================
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
A <- boot.array(boot.out)
theta.b <- boot.out$t
n <- NROW(patch)
jack.se <- numeric(n)
for (i in 1:n) {
  #in i-th replicate omit all samples with x[i]
  keep <- which(A[, i] == 0)
  jack.se[i] <- sd(theta.b[keep])
}
se.bar <- mean(jack.se)
se.se <- sqrt((n-1) * mean((jack.se - se.bar)^2))
print(paste("Jackknife-after-bootstrap est. se(se)=", se.se))
mean(patch$y)/mean(patch$z)

# initialize
data(patch, package = "bootstrap")
y <- patch$y
z <- patch$z
dat <- cbind(y, z)
n <- NROW(dat)
B <- 2000

# jackknife-after-bootstrap step 1: run the bootstrap
theta_boot <- function(dat, ind) {
  # function to compute the statistic
  y <- dat[ind, 1]
  z <- dat[ind, 2]
  mean(y) / mean(z)
}
boot.obj <- boot(dat, statistic = theta_boot, R=2000)
theta.hat <- boot.obj$t0
theta.b <- boot.obj$t
se.boot <- sd(theta.b)
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
mean(patch$y)/mean(patch$z)
