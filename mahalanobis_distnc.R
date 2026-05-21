library(robustbase)
library(ggplot2)
library(mvoutlier)
library(robustbase)
library(rrcov)
library(DescTools)

set.seed(42)
n <- 100
# Clean data: bivariate normal
X <- MASS::mvrnorm(n, mu = c(0, 0), 
                   Sigma = matrix(c(1, 0.8, 0.8, 1), 2, 2))

# Inject 10 outliers
X[91:100, ] <- matrix(c(5, 5), nrow = 10, ncol = 2, byrow = TRUE)
df <- as.data.frame(X)

# --- Classical Mahalanobis ---
cov_classic  <- cov(df)
mean_classic <- colMeans(df)
d2_classic   <- mahalanobis(df, center = mean_classic, cov = cov_classic)

# --- Robust Mahalanobis (MCD) ---
mcd <- covMcd(df)   # from robustbase
d2_robust <- mahalanobis(df, center = mcd$center, cov = mcd$cov)

# using chi.squared threshold
p         <- ncol(df)
threshold <- qchisq(0.975, df = p)   # 97.5th percentile, common choice

outliers_classic <- which(d2_classic > threshold)
outliers_robust  <- which(d2_robust  > threshold)

cat("Classical flagged:", length(outliers_classic), "\n")
cat("Robust flagged:   ", length(outliers_robust),  "\n")

dd_df <- data.frame(
  classical = sqrt(d2_classic),
  robust    = sqrt(d2_robust),
  outlier   = factor(1:n %in% outliers_robust)
)

ggplot(dd_df, aes(x = classical, y = robust, color = outlier)) +
  geom_point(size = 2) +
  geom_vline(xintercept = sqrt(threshold), linetype = "dashed") +
  geom_hline(yintercept = sqrt(threshold), linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
  labs(title = "Distance-Distance Plot",
       x = "Classical Mahalanobis Distance",
       y = "Robust Mahalanobis Distance") +
  theme_minimal()

# Automatic outlier detection with robust Mahalanobis
result <- aq.plot(df)         # plots + returns outlier flags
result$outliers               # logical vector
out <- uni.plot(df, symb = FALSE)

mcd <- covMcd(df)
mcd
mcd$mah        # robust Mahalanobis distances
mcd$weights    # 1 = inlier, 0 = outlier (built-in flagging)


fit <- CovMcd(df)
getDistance(fit)       # robust distances
plot(fit)              # DD plot built in








