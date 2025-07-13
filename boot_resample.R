library(rsample)
library(purrr)
library(tidyr)
library(infer)
library(broom)
library(ggplot2)
set.seed(27)
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)

## model 1
lm_mdl<- function(split){
  lm(mpg~disp+hp+drat,analysis(split))
}
boot_models <- boots %>% 
               mutate(model = map(splits, lm_mdl),
                      coef_info = map(model, tidy))
boot_coefs <- boot_models %>% 
              unnest(coef_info)
percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

## model 2
lm_mdl<- function(split){
  lm(mpg~wt,analysis(split))
}
boot_models <- boots %>% 
  mutate(model = map(splits, lm_mdl),
         coef_info = map(model, tidy))
boot_coefs <- boot_models %>% 
  unnest(coef_info)
percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

boot_aug <- 
  boot_models %>% 
  sample_n(200) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

ggplot(boot_aug, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "blue") +
  geom_point()

## model 3
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


# Out-of-bag error
oob_results <- boots %>%
  mutate(
    model = map(splits, lm_mdl),
    oob_preds = map2(model, splits, ~{
      assess_data <- assessment(.y)
      assess_data$pred <- predict(.x, newdata = assess_data)
      assess_data
    }),
    oob_rmse = map_dbl(oob_preds, ~{
      sqrt(mean((.x$mpg - .x$pred)^2))
    })
  )
hist(oob_results$oob_rmse,breaks="FD")
mean_oob_rmse <- mean(oob_results$oob_rmse)
mean_oob_rmse

# using infer
mtcars %>%
  dplyr::select(mpg) %>%
  specify(response = mpg) %>%
  generate(reps = 10000, type = 'bootstrap') %>%
  calculate(stat = 'mean')  -> bot_ci_df
bot_ci_df
hist(bot_ci_df$stat,breaks="FD")

mtcars %>%
  dplyr::select(hp) %>%
  specify(response = hp) %>%
  generate(reps = 10000, type = 'bootstrap') %>%
  calculate(stat = 'sd')  -> bot_cisd
hist(bot_cisd$stat,breaks="FD")

#jacknife
for (i in 1:nrow(mtcars)) {
  mtcars_minus_one <- mtcars[-i,]
  mean_value_minus_one = mean(mtcars_minus_one$mpg)
  mtcars$jackknife_mean_mpg[i] <- mean_value_minus_one
}
ggplot(mtcars, aes(x = jackknife_mean_mpg)) + 
  geom_histogram(bins = 32, color = 'black', fill = '#ef8a47', alpha = 0.75) +
  labs(title = "Mtcars", subtitle = "Histogram") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(plot.subtitle = element_text(hjust = 0.5))
n <- 32
standard_error <- sd(mtcars$mpg)/sqrt(n)
jacknife_mean_mpg <- mean(mtcars$jackknife_mean_mpg) 
jacknife_standard_error <- sqrt(((n - 1) / n) * sum((mtcars$jackknife_mean_mpg-jacknife_mean_mpg) ^ 2))
t.star <- qt(p=0.975,df=n-1) # what is qt
confidence_intervals <- mean(mtcars$mpg) + c(-1,1) * t.star * standard_error
confidence_intervals
quantile(bot_ci_df$stat,probs=c(0.025,0.9725))
