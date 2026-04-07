# Observed data
n <- 20       # number of flips
k <- 14       # number of heads

# Grid of possible theta values
theta <- seq(0, 1, length.out = 1000)

# Likelihood: L(theta | data) = dbinom(k, n, theta)
likelihood <- dbinom(k, size = n, prob = theta)

# Log-likelihood: log of the above
log_likelihood <- dbinom(k, size = n, prob = theta, log = TRUE)

library(ggplot2)

df <- data.frame(theta, likelihood, log_likelihood)

# Likelihood plot
ggplot(df, aes(x = theta, y = likelihood)) +
  geom_line(color = "#2196F3", linewidth = 1.2) +
  geom_vline(xintercept = k/n, linetype = "dashed", color = "red") +
  annotate("text", x = k/n + 0.05, y = max(likelihood) * 0.9,
           label = paste("MLE =", k/n), color = "red") +
  labs(
    title = "Likelihood Function",
    subtitle = paste(k, "heads in", n, "flips"),
    x = expression(theta ~ "(probability of heads)"),
    y = expression(L(theta ~ "|" ~ data))
  ) +
  theme_minimal(base_size = 14)

# Log-likelihood plot
ggplot(df, aes(x = theta, y = log_likelihood)) +
  geom_line(color = "#4CAF50", linewidth = 1.2) +
  geom_vline(xintercept = k/n, linetype = "dashed", color = "red") +
  annotate("text", x = k/n + 0.05, y = max(log_likelihood) * 1.1,
           label = paste("MLE =", k/n), color = "red") +
  labs(
    title = "Log-Likelihood Function",
    subtitle = paste(k, "heads in", n, "flips"),
    x = expression(theta ~ "(probability of heads)"),
    y = expression(ell(theta) ~ "= log L(theta | data)")
  ) +
  theme_minimal(base_size = 14)


# Analytical MLE for binomial = k/n
mle_analytical <- k / n
cat("Analytical MLE:", mle_analytical, "\n")

# Numerical MLE using optimization
neg_log_lik <- function(theta) {
  -dbinom(k, size = n, prob = theta, log = TRUE)
}

result <- optimize(neg_log_lik, interval = c(0, 1))
cat("Numerical MLE:", result$minimum, "\n")


# Likelihood Ratio
n <- 20
k <- 14

theta_null <- 0.5          # H0: fair coin
theta_mle  <- k / n        # MLE = 0.70

# Log-likelihoods
ll_null <- dbinom(k, size = n, prob = theta_null, log = TRUE)
ll_mle  <- dbinom(k, size = n, prob = theta_mle,  log = TRUE)

cat("Log-likelihood under H0 (theta=0.5):", round(ll_null, 4), "\n")
cat("Log-likelihood under MLE (theta=0.7):", round(ll_mle, 4), "\n")

# LRT statistic
Lambda <- -2 * (ll_null - ll_mle)
cat("LRT statistic (Lambda):", round(Lambda, 4), "\n")

# p-value from chi-squared distribution with 1 df
p_value <- pchisq(Lambda, df = 1, lower.tail = FALSE)
cat("p-value:", round(p_value, 4), "\n")

#---------------------
theta_grid <- seq(0, 1, length.out = 1000)
ll_grid    <- dbinom(k, size = n, prob = theta_grid, log = TRUE)
df         <- data.frame(theta = theta_grid, ll = ll_grid)

# Critical threshold: MLE - chi2(0.95, df=1)/2
critical_drop <- qchisq(0.95, df = 1) / 2  # = 1.92

ggplot(df, aes(x = theta, y = ll)) +
  geom_line(color = "#4CAF50", linewidth = 1.2) +
  
  # MLE point
  geom_point(aes(x = theta_mle, y = ll_mle),
             color = "red", size = 3) +
  annotate("text", x = theta_mle + 0.06, y = ll_mle,
           label = "MLE = 0.70", color = "red", size = 3.5) +
  
  # H0 point
  geom_point(aes(x = theta_null, y = ll_null),
             color = "blue", size = 3) +
  annotate("text", x = theta_null - 0.07, y = ll_null,
           label = "H0: θ=0.5", color = "blue", size = 3.5) +
  
  # Vertical drop showing the LRT gap
  geom_segment(aes(x = theta_null, xend = theta_null,
                   y = ll_null, yend = ll_mle),
               color = "purple", linewidth = 1, linetype = "dashed") +
  annotate("text", x = theta_null + 0.08, y = (ll_null + ll_mle) / 2,
           label = paste("Λ/2 =", round(Lambda/2, 2)),
           color = "purple", size = 3.5) +
  
  # 95% confidence region (likelihood ratio interval)
  geom_hline(yintercept = ll_mle - critical_drop,
             linetype = "dotted", color = "orange", linewidth = 1) +
  annotate("text", x = 0.1, y = ll_mle - critical_drop + 0.15,
           label = "95% LR interval threshold", color = "orange", size = 3) +
  
  labs(
    title = "Log-Likelihood with LRT Illustrated",
    subtitle = "Gap between H0 and MLE drives the test statistic",
    x = expression(theta),
    y = expression(ell(theta))
  ) +
  theme_minimal(base_size = 14)







