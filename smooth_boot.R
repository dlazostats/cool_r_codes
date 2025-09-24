## spline and bootstrap
#------------------------
library(rsample)
library(purrr)
library(lattice)

set.seed(123)
n <- 250
x <- seq(0, 10, length.out = n)
y <- 2 * sin(x) + 0.5 * x + rnorm(n, 0, 0.5)
data <- data.frame(x = x, y = y)
ml_spl<-smooth.spline(x, y)
plot(x,y)
lines(ml_spl,col="red")
ml_spl$df

#bootstrap
boot_s<-bootstraps(data,time=500,apparent = T)
fit_spline <- function(split) {
  data <- analysis(split)
  smooth.spline(data$x, data$y, df = 10)
}

boot_splines <- boot_s %>% 
                mutate(spline = map(splits, fit_spline),
                       aug_train = map(spline, augment))

splines_aug <-boot_splines %>% 
              unnest(aug_train)

sd(splines_aug$.fitted) # sd of the fitted  values

splines_aug %>% 
  group_by(id) %>% 
  summarize(sd_fitted=sd(.fitted)) ->sd_boot

histogram(sd_boot$sd_fitted)

ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, col = "blue") +
  geom_point()
