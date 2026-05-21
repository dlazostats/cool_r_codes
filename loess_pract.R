library(ggplot2)

# --- 1. Generate example data ---
set.seed(42)
x <- seq(-pi, pi, length.out = 80)
y <- sin(x) + rnorm(80, sd = 0.4)

# --- 2. Fit LOESS models with different spans ---
fit_narrow <- loess(y ~ x, span = 0.25, degree = 1)  # wiggly
fit_default <- loess(y ~ x, span = 0.75, degree = 1) # R default
fit_wide    <- loess(y ~ x, span = 0.90, degree = 2) # smooth, quadratic

# --- 3. Predict on a fine grid ---
x_grid <- seq(-pi, pi, length.out = 200)

pred_narrow  <- predict(fit_narrow,  newdata = data.frame(x = x_grid))
pred_default <- predict(fit_default, newdata = data.frame(x = x_grid))
pred_wide    <- predict(fit_wide,    newdata = data.frame(x = x_grid))

# --- 4. Plot ---
{
  plot(x, y, pch = 16, col = "gray60", main = "LOESS with different spans",
       xlab = "x", ylab = "y")
  lines(x_grid, sin(x_grid),    col = "black",  lty = 2, lwd = 1.5) # true signal
  lines(x_grid, pred_narrow,    col = "tomato",  lwd = 2)
  lines(x_grid, pred_default,   col = "steelblue", lwd = 2)
  lines(x_grid, pred_wide,      col = "seagreen", lwd = 2)
  legend("topleft",
         legend = c("True sin(x)", "span=0.25", "span=0.75 (default)", "span=0.90 deg=2"),
         col = c("black","tomato","steelblue","seagreen"),
         lty = c(2,1,1,1), lwd = 2, cex = 0.85)
}

# --- 5. Inspect the fit object ---
summary(fit_default)
# → shows residual standard error, equivalent degrees of freedom, etc.

df <- data.frame(x, y)
ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.4, se = TRUE) + # se = confidence band
  stat_function(fun = sin, linetype = "dashed", color = "tomato") +
  labs(title = "LOESS via ggplot2", subtitle = "Dashed = true sin(x)")

u=d(xi,xo)/farthest
u<-0.5
(1-abs(u)^3)^3
