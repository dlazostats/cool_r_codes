library(ggplot2)
library(mgcv)

# ── 1. Generate data ──────────────────────────────────────────────────────────
set.seed(42)
n     <- 80
noise <- 0.6

x    <- runif(n, min = -1, max = 5)
y    <- sin(2 * x) + 0.5 * x + rnorm(n, mean = 0, sd = noise)
data <- data.frame(x = x, y = y)

# true function for reference
true_curve <- data.frame(
  x = seq(-1, 5, length.out = 300),
  y = sin(2 * seq(-1, 5, length.out = 300)) + 0.5 * seq(-1, 5, length.out = 300)
)

# ── 2. Fit the models ─────────────────────────────────────────────────────────

# smoothing spline — lambda chosen by cross-validation
fit_spline <- smooth.spline(x, y, cv = TRUE)

# LOESS — span is the bandwidth (fraction of data used locally)
fit_loess  <- loess(y ~ x, data = data, span = 0.4)

# linear regression
fit_lm     <- lm(y ~ x, data = data)

# GAM spline (via mgcv) — closest to the theoretical smoothing spline
fit_gam    <- gam(y ~ s(x), data = data, method = "REML")

# ── 3. Predictions on a fine grid ─────────────────────────────────────────────
x_grid <- seq(-1, 5, length.out = 300)

pred_df <- data.frame(
  x       = x_grid,
  spline  = predict(fit_spline, x_grid)$y,
  loess   = predict(fit_loess,  newdata = data.frame(x = x_grid)),
  linear  = predict(fit_lm,     newdata = data.frame(x = x_grid)),
  gam     = predict(fit_gam,    newdata = data.frame(x = x_grid)),
  truth   = sin(2 * x_grid) + 0.5 * x_grid
)

# ── 4. Plot ───────────────────────────────────────────────────────────────────
ggplot() +
  # data points
  geom_point(data = data,
             aes(x = x, y = y),
             alpha = 0.4, size = 1.8, color = "#378ADD") +
  # true function
  geom_line(data = true_curve,
            aes(x = x, y = y),
            linetype = "dashed", color = "gray50", linewidth = 0.8) +
  # smoothing spline
  geom_line(data = pred_df,
            aes(x = x, y = spline, color = "Smoothing spline"),
            linewidth = 1.1) +
  # LOESS
  geom_line(data = pred_df,
            aes(x = x, y = loess, color = "LOESS"),
            linewidth = 1.1) +
  # linear
  geom_line(data = pred_df,
            aes(x = x, y = linear, color = "Linear"),
            linewidth = 1.0) +
  # GAM
  geom_line(data = pred_df,
            aes(x = x, y = gam, color = "GAM spline"),
            linewidth = 1.0, linetype = "dotted") +
  scale_color_manual(
    name   = "Method",
    values = c(
      "Smoothing spline" = "#378ADD",
      "LOESS"            = "#D85A30",
      "Linear"           = "#639922",
      "GAM spline"       = "#7F77DD"
    )
  ) +
  labs(
    title    = "Smoothing spline vs LOESS vs linear regression",
    subtitle = "Dashed gray = true function: sin(2x) + 0.5x",
    x        = "x",
    y        = "y"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ── 5. Inspect the fitted models ──────────────────────────────────────────────

# chosen lambda by CV
cat("Lambda chosen by CV:", fit_spline$lambda, "\n")

# effective degrees of freedom (higher = more flexible)
cat("Spline df:", fit_spline$df, "\n")
cat("GAM df:   ", sum(fit_gam$edf), "\n")

# GAM summary — shows significance of the smooth term
summary(fit_gam)

# ── 6. Try different lambda values manually ───────────────────────────────────
lambdas <- c(0.001, 0.05, 0.5, 2)

lambda_preds <- do.call(rbind, lapply(lambdas, function(lam) {
  fit <- smooth.spline(x, y, lambda = lam)
  data.frame(
    x      = x_grid,
    y      = predict(fit, x_grid)$y,
    lambda = paste0("λ = ", lam)
  )
}))

ggplot() +
  geom_point(data = data,
             aes(x = x, y = y),
             alpha = 0.3, size = 1.5, color = "gray40") +
  geom_line(data = true_curve,
            aes(x = x, y = y),
            linetype = "dashed", color = "gray50") +
  geom_line(data = lambda_preds,
            aes(x = x, y = y, color = lambda),
            linewidth = 1.0) +
  labs(
    title    = "Effect of λ on the smoothing spline",
    subtitle = "Small λ = wiggly (overfitting), large λ = flat (underfitting)",
    color    = "Lambda",
    x        = "x", y = "y"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

library(caret)
ind<-createDataPartition(true_curve$y,list=F,p=0.2)
train<-true_curve[-ind,]
test<-true_curve[ind,]

fit  <- gam(y ~ s(x), data = train, method = "REML")
pred <- predict(fit, newdata = test, se.fit = TRUE)

plot(test$y)
lines(pred$fit+1.96*pred$se.fit,col="red")
lines(pred$fit,col="blue")
lines(pred$fit-1.96*pred$se.fit,col="red")

fit <- gam(y ~ s(x), data = data, method = "REML")
par(mfrow = c(2,2))
gam.check(fit)

diag_df <- data.frame(
  fitted    = fitted(fit),
  residuals = residuals(fit)
)

ggplot(diag_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5, color = "#378ADD") +
  geom_hline(yintercept = 0,
             color = "#D85A30", linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE,
              color = "gray40", linewidth = 0.8) +
  labs(title    = "Residuals vs fitted values",
       subtitle = "Should show no pattern if assumptions hold",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 13)

# ── 4. formal normality test ──────────────────────────────────────────────────
shapiro.test(residuals(fit)) 





