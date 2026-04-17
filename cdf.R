library(boot)
library(ACSWR)

# Load the nerve data
data(nerve)
x <- nerve

# Compute empirical CDF
n <- length(x)
x_sorted <- sort(x)

# KS confidence band (simultaneous, 95%)
alpha <- 0.05
# Critical value from KS distribution
D <- 1.36 / sqrt(n)  # ~0.95 quantile of KS statistic

# ECDF values
ecdf_fn <- ecdf(x)

# Plot limits
x_seq <- seq(0, 1.5, length.out = 1000)

# Set up plot
plot(ecdf_fn, 
     xlim = c(0, 1.5), 
     ylim = c(0, 1.0),
     main = "",
     xlab = "",
     ylab = "",
     col = "black",
     lwd = 1.5,
     do.points = FALSE,
     verticals = TRUE)

# Add confidence bands
upper <- function(x) pmin(ecdf_fn(x) + D, 1)
lower <- function(x) pmax(ecdf_fn(x) - D, 0)

lines(x_seq, upper(x_seq), col = "red", lty = 2, lwd = 1.5)
lines(x_seq, lower(x_seq), col = "red", lty = 2, lwd = 1.5)

# Add rug
rug(x)

# Add caption
mtext("FIGURE 7.1. Nerve data. Each vertical line represents one data point. The solid",
      side = 1, line = 3.5, cex = 0.75, adj = 0)
mtext("line is the empirical distribution function. The lines above and below the middle",
      side = 1, line = 4.5, cex = 0.75, adj = 0)
mtext("line are a 95 percent confidence band.",
      side = 1, line = 5.5, cex = 0.75, adj = 0)

# approx mean
pts <- knots(ecdf_fn)
sum(diff(c(0, ecdf_fn(pts))) * pts)
mean(x_sorted)
