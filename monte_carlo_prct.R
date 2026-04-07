library(combinat)
library(datasets)
library(boot)
library(mcsm)
library(coin)
library(infer)

# basic stats
x<-rnorm(25)
t.test(x)
cor.test(faithful[,1],faithful[,2])
glm(formula = type ~ bmi + age, family = "binomial", data = Pima.tr)

# warm-up
## Bootstrap
set.seed(42)
x<-rexp(50,rate=0.5)
B<-10000
boot_med<-replicate(B, median(sample(x,replace = T)))
ci <- quantile(boot_med, c(0.025, 0.975))
medfun<-function(data,index){ median(data[index])}
boot_med_bt<-boot(x,medfun,B)
boot_med_bt
ci
boot.ci(boot_med_bt)

## Permutation test
set.seed(1)
g1 <- rnorm(30, mean = 5, sd = 2)
g2 <- rnorm(30, mean = 6, sd = 2)
obs_diff <- mean(g1) - mean(g2)
combined <- c(g1, g2)

B <- 10000
perm_diffs <- replicate(B, {
  perm <- sample(combined)
  mean(perm[1:30]) - mean(perm[31:60])
})
p_val <- mean(abs(perm_diffs) >= abs(obs_diff))
cat("Permutation p-value:", p_val, "\n")

group <- factor(rep(c("A","B"), each = 30))
values <- c(g1, g2)
oneway_test(values ~ group, distribution = "approximate")

df <- data.frame(
  values = c(g1, g2),
  group = rep(c("A", "B"), each = 30)
)
obs_stat <- df %>%
  specify(values ~ group) %>%
  calculate(stat = "diff in means", order = c("A", "B"))
null_dist <-df %>%
  specify(values ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("A", "B")) 
p_val <- null_dist %>%
  get_p_value(obs_stat = obs_stat, direction = "two_sided")
p_val

null_dist %>%
  visualize() +
  shade_p_value(obs_stat = obs_stat, direction = "two_sided")

## power analysis via simulation
simulate_power <- function(n, delta, sd = 1, alpha = 0.05, B = 5000) {
  rejections <- replicate(B, {
    x <- rnorm(n, mean = 0,     sd = sd)
    y <- rnorm(n, mean = delta,  sd = sd)
    t.test(x, y)$p.value < alpha
  })
  mean(rejections)
}
ns <- seq(10, 100, by = 10)
powers <- sapply(ns, simulate_power, delta = 0.5)
plot(ns, powers, type = "b", pch = 19,
     xlab = "Sample size per group", ylab = "Estimated Power",
     main = "Power curve (delta = 0.5, alpha = 0.05)")
abline(h = 0.80, lty = 2, col = "red")

## Double Bootstrap
#------------------
double_bootstrap_ci <- function(x, stat_fn, B = 1000, M = 200,
                                alpha = 0.05, seed = 42) {
  set.seed(seed)
  n <- length(x)
  theta_hat <- stat_fn(x)
  
  # --- Outer bootstrap ---
  theta_star <- numeric(B)
  # For each outer sample, store the inner quantile of theta_star relative
  # to its own inner distribution
  inner_quantiles <- numeric(B)
  
  for (b in seq_len(B)) {
    x_star <- sample(x, replace = TRUE)
    theta_star[b] <- stat_fn(x_star)
    
    # --- Inner bootstrap (nested inside outer) ---
    theta_double <- replicate(M, stat_fn(sample(x_star, replace = TRUE)))
    
    # Where does theta_hat fall in the inner distribution?
    # This gives us the "coverage" of the inner CI
    inner_quantiles[b] <- mean(theta_double <= theta_hat)
  }
  
  # --- Calibrate alpha ---
  # Find alpha* such that quantile(inner_quantiles, alpha*) ≈ alpha/2
  # i.e., how extreme does theta_hat need to be in the inner dist?
  alpha_star_lo <- quantile(inner_quantiles, probs = alpha / 2)
  alpha_star_hi <- quantile(inner_quantiles, probs = 1 - alpha / 2)
  
  # --- Construct calibrated CI from outer bootstrap ---
  ci_calibrated <- quantile(theta_star, probs = c(alpha_star_lo, alpha_star_hi))
  
  # Standard percentile CI for comparison
  ci_standard <- quantile(theta_star, probs = c(alpha / 2, 1 - alpha / 2))
  
  list(
    estimate      = theta_hat,
    ci_standard   = ci_standard,
    ci_calibrated = ci_calibrated,
    alpha_star    = c(alpha_star_lo, alpha_star_hi)
  )
}
set.seed(99)
x <- rexp(30, rate = 1)   # n=30, skewed — standard bootstrap struggles here

result <- double_bootstrap_ci(x, stat_fn = mean, B = 1000, M = 200)
cat("True mean (rate=1):    1.000\n")
cat("Estimate:             ", round(result$estimate, 3), "\n\n")
cat("Standard 95% CI:      [", round(result$ci_standard, 3), "]\n")
cat("Calibrated 95% CI:    [", round(result$ci_calibrated, 3), "]\n")
cat("Effective alpha used: [", round(result$alpha_star, 4), "]\n")

simulate_coverage <- function(n, B = 500, M = 100, reps = 300,
                              true_param = 1) {
  cover_std  <- logical(reps)
  cover_cal  <- logical(reps)
  
  for (i in seq_len(reps)) {
    x <- rexp(n, rate = 1)
    res <- double_bootstrap_ci(x, mean, B = B, M = M, seed = i)
    cover_std[i] <- res$ci_standard[1]   <= true_param &
      true_param <= res$ci_standard[2]
    cover_cal[i] <- res$ci_calibrated[1] <= true_param &
      true_param <= res$ci_calibrated[2]
  }
  
  cat(sprintf("n = %d | Standard: %.1f%% | Calibrated: %.1f%%\n",
              n,
              100 * mean(cover_std),
              100 * mean(cover_cal)))
}
# Compare coverage at different sample sizes
simulate_coverage(n = 15)
simulate_coverage(n = 30)






