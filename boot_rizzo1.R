# Statistical Computing - Rizzo
# bootstrap
#-------------------------------
library(boot)
library(tidymodels)
library(rsample)

# boot
#-------
# Example 1
dta<-read.delim("clipboard")
rt<-cor(dta$LSAT,dta$GPA)

## usando teoria
se_rt<-(1-rt^2)/(sqrt(length(dta$GPA)-2))
cat("correlacion de ",rt, " con error standar teorico de ",se_rt)

## usando bootstrap
coef_cor<-numeric()
for(i in 1:200){
  dtb<-dta[sample(1:nrow(dta),replace = T),]
  rt<-cor(dtb$LSAT,dtb$GPA)
  coef_cor[i]<-rt
}
sd(coef_cor) #bootstrap estimate of se(R)

#using boot
r<-function(x,i){
  cor(x[i,1],x[i,2])
}
cor(dta$LSAT,dta$GPA)
rboot<-boot(data=dta,statistic=r,R=200)
boot.ci(rboot)
boot.ci(rboot,type="bca")
rboot
sd(rboot$t)
mean(rboot$t)-cor(dta$LSAT,dta$GPA)
plot(rboot) 

# Example 2
data(patch, package = "bootstrap")
patch




# bootstrap and tidymodels
#-------------------------
ggplot(mtcars, aes(mpg, wt)) + 
  geom_point()

# nonlinear least squares
nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
summary(nlsfit)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_line(aes(y = predict(nlsfit)))

set.seed(27)
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
boots

fit_nls_on_bootstrap <- function(split) {
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
}

boot_models <-boots %>% 
              mutate(model = map(splits, fit_nls_on_bootstrap),
                     coef_info = map(model, tidy))

boot_coefs <- boot_models %>% 
              unnest(coef_info)

## confidence intervals
percentile_intervals <- int_pctl(boot_models, coef_info,alpha=0.1)
percentile_intervals

# histogram boot resamples
ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

# uncertainty in the fitted curve
#----------------------------------
boot_aug <- boot_models %>% 
            sample_n(200) %>% 
            mutate(augmented = map(model, augment)) %>% 
            unnest(augmented)

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
  geom_point()

# using spline
#-------------
fit_spline_on_bootstrap <- function(split) {
  data <- analysis(split)
  smooth.spline(data$wt, data$mpg, df = 4)
}
boot_splines <- boots %>% 
                sample_n(200) %>% 
                mutate(spline = map(splits, fit_spline_on_bootstrap),
                       aug_train = map(spline, augment))
splines_aug <- boot_splines %>% 
               unnest(aug_train)
ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, col = "blue") +
  geom_point()








