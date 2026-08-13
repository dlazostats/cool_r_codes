library(splines)

if (requireNamespace("faraway", quietly = TRUE)) {
  data(ozone, package = "faraway")
} else {
  if (!file.exists("ozone.rda"))
    download.file(paste0("https://raw.githubusercontent.com/julianfaraway/",
                         "faraway/master/data/ozone.rda"), "ozone.rda")
  load("ozone.rda")
}

## paper: "the original 330 data points were collapsed onto 128 points
## with unique values of Daggot Pressure Gradient"
g <- aggregate(O3 ~ dpg, data = ozone, FUN = mean)
g <- g[order(g$dpg), ]
x <- g$dpg; y <- g$O3; n <- length(x)
cat("n =", n, "\n")
plot(x,y)
dtozone<-data.frame(x=x,y=y)

# running mean
R <- diff(range(x))
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(ksmooth(x, y, "box", bandwidth = 0.27*R, x.points = x), col = "red",  lwd = 2)
legend("topleft", c( "running mean (k=47)"),
       col = c("red"), lwd = 2, bty = "n")

# bin smooother
K <- 5
b <- cut(x, breaks = K)              # K equal-width bins, done for you
fit <- fitted(lm(y ~ b))
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(x, fit, type = "s", col = "red", lwd = 2)
legend("topleft", c( "bin smoother"),
       col = c("red"), lwd = 2, bty = "n")

# linear smoother
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(supsmu(x, y, span = 0.27), col = "red", lwd = 2)

# lowess
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(lowess(x, y, f = 0.27, iter = 0), col = "red", lwd = 2)

# cubic spline
m_bs <- lm(y ~ bs(x, df = 6))  
xg <- seq(min(x), max(x), length.out = 300)
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(xg, predict(m_bs, data.frame(x = xg)), col = "red",  lwd = 2)

# gaussian kernel
plot(x, y, col = "grey60", pch = 16, xlab = "dpg", ylab = "O3")
lines(ksmooth(x, y, "normal", bandwidth = 10, x.points = x), col = "purple", lwd = 2, lty = 2)

## all of them
{
  plot(x, y, col = "grey70", pch = 16, xlab = "dpg", ylab = "O3")
  lines(x, ave(y, cut(x, 8), FUN = mean), type = "s", col = "grey40", lwd = 2)
  lines(ksmooth(x, y, "box", bandwidth = 0.27*diff(range(x)), x.points = x), col = 2, lwd = 2)
  lines(supsmu(x, y, span = 0.27), col = "red", lwd = 2)
  lines(lowess(x, y, f = 0.27),                         col = 4, lwd = 2)
  lines(smooth.spline(x, y),                            col = 5, lwd = 2)
  lines(ksmooth(x, y, "normal", bandwidth = 10, x.points = x), col = "purple", lwd = 2, lty = 2)
  legend("topleft", c("bin","running mean","running line","lowess","smooth spline","gaussian kernel"),
         col = c("grey40", 2:6), lwd = 2, bty = "n", cex = 0.8)
}
