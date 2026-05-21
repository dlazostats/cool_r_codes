# ============================================================
# Bootstrap Methods Comparison in R
# 1. Regular (Frequentist) Bootstrap
# 2. Bayesian Bootstrap (Rubin, 1981)
# 3. Smoothed Bayesian Bootstrap
# ============================================================

set.seed(42)
library(ggplot2)
library(dplyr)
library(tidyr)

# ── Sample Data ──────────────────────────────────────────────
# Skewed data: reaction times (ms) from a small experiment
n <- 30
data_obs <- c(210, 245, 198, 312, 267, 289, 223, 301, 255, 278,
              190, 330, 241, 268, 215, 295, 260, 240, 275, 310,
              205, 285, 250, 320, 232, 262, 291, 218, 274, 299)
{
  cat("── Original Data Summary ──\n")
  cat(sprintf("  n     = %d\n", n))
  cat(sprintf("  Mean  = %.2f ms\n", mean(data_obs)))
  cat(sprintf("  SD    = %.2f ms\n", sd(data_obs)))
  cat(sprintf("  Range = [%.0f, %.0f] ms\n\n", min(data_obs), max(data_obs)))
}

B <- 5000  # number of bootstrap iterations

# ============================================================
# 1. REGULAR (FREQUENTIST) BOOTSTRAP
# ============================================================
# Resample with replacement; each obs gets weight 0 or 1/n.
regular_boot <- function(x, B = 5000, statistic = mean) {
  replicate(B, statistic(sample(x, length(x), replace = TRUE)))
}

reg_means <- regular_boot(data_obs, B)
{
  cat("── Regular Bootstrap ──\n")
  cat(sprintf("  Mean of bootstrap dist : %.3f\n", mean(reg_means)))
  cat(sprintf("  Bootstrap SE           : %.3f\n", sd(reg_means)))
  cat(sprintf("  95%% CI                 : [%.3f, %.3f]\n\n",
              quantile(reg_means, 0.025), quantile(reg_means, 0.975)))  
}

# ============================================================
# 2. BAYESIAN BOOTSTRAP (Rubin, 1981)
# ============================================================
# Instead of integer counts, draw continuous Dirichlet weights.
# w ~ Dirichlet(1, ..., 1)  ↔  normalised Exponential(1) draws.
bayesian_boot <- function(x, B = 5000, statistic = mean) {
  n <- length(x)
  replicate(B, {
    w <- rexp(n, rate = 1)
    w <- w / sum(w)           # Dirichlet(1,...,1) weights
    statistic(sample(x, n, replace = TRUE, prob = w))
  })
}

bay_means <- bayesian_boot(data_obs, B)

{
  cat("── Bayesian Bootstrap ──\n")
  cat(sprintf("  Mean of bootstrap dist : %.3f\n", mean(bay_means)))
  cat(sprintf("  Bootstrap SE           : %.3f\n", sd(bay_means)))
  cat(sprintf("  95%% Credible Interval  : [%.3f, %.3f]\n\n",
              quantile(bay_means, 0.025), quantile(bay_means, 0.975)))  
}

# ============================================================
# 3. SMOOTHED BAYESIAN BOOTSTRAP
# ============================================================
# Adds Gaussian smoothing (bandwidth h) to the Bayesian bootstrap.
# Each replicate: draw Dirichlet weights, then perturb each
# selected value with N(0, h²) noise → smoother posterior.
smoothed_bayesian_boot <- function(x, B = 5000, statistic = mean,
                                   h = NULL) {
  n   <- length(x)
  # Silverman's rule-of-thumb bandwidth if not supplied
  if (is.null(h)) h <- 1.06 * sd(x) * n^(-1/5)
  cat(sprintf("  Bandwidth h (Silverman) = %.3f\n", h))
  
  replicate(B, {
    w    <- rexp(n, rate = 1); w <- w / sum(w)
    samp <- sample(x, n, replace = TRUE, prob = w)
    samp <- samp + rnorm(n, mean = 0, sd = h)   # smooth
    statistic(samp)
  })
}

{
  cat("── Smoothed Bayesian Bootstrap ──\n")
  smooth_means <- smoothed_bayesian_boot(data_obs, B)
  cat(sprintf("  Mean of bootstrap dist : %.3f\n", mean(smooth_means)))
  cat(sprintf("  Bootstrap SE           : %.3f\n", sd(smooth_means)))
  cat(sprintf("  95%% Credible Interval  : [%.3f, %.3f]\n\n",
              quantile(smooth_means, 0.025), quantile(smooth_means, 0.975)))  
}

# ============================================================
# COMPARISON TABLE
# ============================================================
results <- data.frame(
  Method = c("Regular Bootstrap", "Bayesian Bootstrap",
             "Smoothed Bayesian Bootstrap"),
  Mean_of_Dist = c(mean(reg_means),   mean(bay_means),   mean(smooth_means)),
  SE           = c(sd(reg_means),     sd(bay_means),     sd(smooth_means)),
  CI_Lower     = c(quantile(reg_means,   0.025),
                   quantile(bay_means,   0.025),
                   quantile(smooth_means, 0.025)),
  CI_Upper     = c(quantile(reg_means,   0.975),
                   quantile(bay_means,   0.975),
                   quantile(smooth_means, 0.975))
)

cat("── Full Comparison ──\n")
print(results, digits = 4, row.names = FALSE)

# ============================================================
# PLOT: Overlapping density curves
# ============================================================
df_long <- data.frame(
  value  = c(reg_means, bay_means, smooth_means),
  Method = rep(c("Regular Bootstrap",
                 "Bayesian Bootstrap",
                 "Smoothed Bayesian Bootstrap"),
               each = B)
)

p <- ggplot(df_long, aes(x = value, fill = Method, colour = Method)) +
  geom_density(alpha = 0.30, linewidth = 0.9) +
  geom_vline(xintercept = mean(data_obs),
             linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate("text", x = mean(data_obs) + 0.5, y = Inf,
           label = paste0("Observed\nmean = ", round(mean(data_obs), 1)),
           hjust = 0, vjust = 1.5, size = 3.5) +
  scale_fill_manual(values  = c("#E63946", "#457B9D", "#2A9D8F")) +
  scale_colour_manual(values = c("#E63946", "#457B9D", "#2A9D8F")) +
  labs(
    title    = "Bootstrap Distribution of the Mean",
    subtitle = "Regular vs Bayesian vs Smoothed Bayesian (B = 5 000)",
    x        = "Bootstrap Mean (ms)",
    y        = "Density",
    fill     = "Method", colour = "Method",
    caption  = "Dashed line = observed sample mean"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40"))

ggsave("bootstrap_comparison.png", plot = p,
       width = 8, height = 5, dpi = 150)

cat("\nPlot saved to bootstrap_comparison.png\n")