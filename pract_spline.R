library(splines)
library(ggplot2)
library(dplyr)
library(jtools)
library(earth)
library(caret)
library(gam)
library(MASS)

# Data
set.seed(42)
n<-350
x <- seq(0, 10, length.out = n)
y <- sin(x) + 0.3 * x + rnorm(n, sd = 0.3)
df <- data.frame(x = x, y = y)
plot(df$x,df$y)

# linear model
lm1<-lm(y~x,data=df)
summ(lm1)
ggplot(df,aes(x=x,y=y))+
  geom_point(colour = "grey60") +
  geom_smooth(method="lm",se=F) +
  theme_minimal()

# Cubic B-spline with 6 degrees of freedom
bsplin1 <- lm(y ~ bs(x, df = 6, degree = 3), data = df)
summary(bsplin1)
ggplot(df, aes(x, y)) +
  geom_point(colour = "grey60") +
  geom_smooth(method = "lm",
              formula = y ~ bs(x, df = 6, degree = 3),
              se = F, colour = "blue")+
  theme_minimal()

# natural spline (the kind with linear behavior beyond the boundaries)
nspln <- lm(y ~ ns(x, df = 6), data = df)
summary(nspln)
ggplot(df, aes(x, y)) +
  geom_point(colour = "grey60") +
  geom_smooth(method = "lm",
              formula = y ~ ns(x, df = 6),
              se = F, colour = "blue")+
  theme_minimal()

ggplot(df, aes(x, y)) +
  geom_point(colour = "grey60") +
  geom_smooth(method = "lm",
              formula = y ~ bs(x, df = 6, degree = 3),
              se = F, colour = "red")+
  geom_smooth(method = "lm",
              formula = y ~ ns(x, df = 6),
              se = F, colour = "blue")+
  theme_minimal()

## predict
x_grid <- data.frame(x = seq(0, 10, length.out = 200))
preds <- predict(nspln, newdata = x_grid)
plot(df$x, df$y)
lines(x_grid$x, preds, col = "green", lwd = 2)


# loess
lo <- loess(y ~ x, data = df, span = 0.25)
ss$lambda; ss$df                   # what it picked
lines(df$x, predict(ss, df$x)$y)


# smoothing spline
ss <- smooth.spline(df$x, df$y) 
df %>% 
  mutate(pred=predict(ss, df$x)$y) %>% 
  ggplot(aes(x = x,y = y)) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = pred, colour = "smooth splin"), linewidth = 1) +
  theme_minimal()


# basic mars
mmars <- earth(y ~ x, data = df)
summary(mmars)
df %>% 
  mutate(pred=predict(mmars, df)) %>% 
  ggplot(aes(x = x,y = y)) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = pred, colour = "MARS fit"), linewidth = 1) +
  theme_minimal()

# advanced mars
# nk = max number of model terms before pruning; raise it so MARS can
# place enough knots to follow all the oscillations across [0, 10]
grid <- expand.grid(degree = 1:3, nprune = 2:20)
ctrl <- trainControl(method = "cv", number = 10)
tuned <- train(y ~ x, data = df,
               method = "earth",
               tuneGrid = grid,
               trControl = ctrl)
densityplot(tuned)
df %>% 
  mutate(pred=predict(tuned, df)) %>% 
  ggplot(aes(x = x,y = y)) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = pred, colour = "MARS fit"), linewidth = 1) +
  theme_minimal()

## Another Example
#------------------------------------------------------------------------------------------------------
mod_df2 = gam(medv ~ bs(lstat, df = 3), data = Boston)
summary(mod_df2)
plot(mod_df2, residuals = TRUE, col = "blue", cex = .5, 
     xlab="lstat: lower status of the population (percent)", 
     ylab="medv: median value owner-occupied homes")
mod_df5 = gam(medv ~ bs(lstat, df = 5), data = Boston)
summary(mod_df5)
plot(mod_df5, residuals = TRUE, col = "blue", cex = .5, 
     xlab="lstat: lower status of the population (percent)", 
     ylab="medv: median value owner-occupied homes")
mod_df8 = gam(medv ~ bs(lstat, df = 8), data = Boston)
summary(mod_df8)
plot(mod_df8, residuals = TRUE, col = "blue", cex = .5, 
     xlab="lstat: lower status of the population (percent)", 
     ylab="medv: median value owner-occupied homes")
c("df=2"=BIC(mod_df2), "df=5"=BIC(mod_df5), "df=8"=BIC(mod_df8))

# natural spline
ns3 = ns(Boston$lstat, df = 3)
attr(ns3, "knots") ## (e) Where are the knots located for the 3 df natural spline?

mod_ns3 = gam(medv ~ ns(lstat, df = 3), data = Boston)
summary(mod_ns3)
plot(mod_ns3, residuals = TRUE, col = "blue", cex = .5)

ns5_x = ns(Boston$lstat, df = 5)
attr(ns5_x, "knots")

matplot(
  Boston$lstat[order(Boston$lstat)],
  ns5_x[order(Boston$lstat),],
  type = "l", lwd = 2,
  xlab = "x: lower status of the population (percent)",
  ylab = expression(b[k](x))
)


mod_ns5 = gam(medv ~ ns(lstat, df = 5), data = Boston)
summary(mod_ns5)
plot(mod_ns5, residuals = TRUE, col = "blue", cex = .5)

c("df=2"=BIC(mod_ns3), "df=5"=BIC(mod_ns5))

mod_sc = gam(medv ~ s(lstat, df = 100), data = Boston) # too complex gam
summary(mod_sc)
plot(mod_sc, residuals = TRUE, cex = .5, col = "blue")


# GAM
#-------------------------------------------------------------------------------------
vars = c("medv", "lstat", "rm", "ptratio", "crim", "dis")
plot(Boston[ , vars])
detach("package:gam", unload = TRUE)
library("mgcv")
boston_GAM = gam(medv ~ s(lstat) + s(rm) + s(ptratio) + s(crim) + s(dis),
                 data = Boston, method = "REML")
summary(boston_GAM)
par(mfrow = c(2, 3))
plot(boston_GAM, residuals = TRUE)

boston_GAM2 = gam(medv ~ s(lstat) + s(rm) + s(ptratio) + s(crim) + s(dis),
                  data = Boston)
summary(boston_GAM2)
par(mfrow = c(2, 3))
plot(boston_GAM2, residuals = TRUE)
