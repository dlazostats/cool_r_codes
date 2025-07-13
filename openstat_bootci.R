library(tidyverse)
library(tidymodels)
library(openintro)

yawn |>
  count(group, result) |>
  group_by(group) |>
  mutate(p_hat = n / sum(n))

ggplot(duke_forest, aes(x = area)) +
  geom_histogram(binwidth = 250)

#95% confidence interval by BT
set.seed(12345)
head(duke_forest)
df_araa_median_boot <- duke_forest |>
  specify(response = area) |>
  generate(reps = 100, type = "bootstrap") |>
  calculate(stat = "median")

df_araa_median_boot |>
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

df_price_area_fit <- linear_reg() |>
  fit(price ~ area, data = duke_forest)

tidy(df_price_area_fit)

#CI for linear regression
df_price_area_boot <- duke_forest |>
  specify(price ~ area) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "slope")

ggplot(df_price_area_boot, aes(x = stat)) +
  geom_histogram(binwidth = 10)

df_price_area_boot |>
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )
lm_m<-lm(price ~ area,data=duke_forest)
summary(lm_m)
confint(lm_m)

obs_stat_abb <- duke_forest |>
  specify(response = area) |>
  calculate(stat = "mean") 
df_araa_median_boot |>
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )

library(kableExtra)

df_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(price ~ area, data = duke_forest)

tidy(df_fit) %>%
  kable(digits = 2)

boot_fits <- duke_forest %>%
  specify(price ~ area) %>%
  generate(reps = 100, type = "bootstrap") %>%
  fit()

boot_fits
ggplot(boot_fits,aes(x=estimate))+
  geom_histogram()+
  facet_wrap(~term,scales="free")

observed_fit <- duke_forest %>%
  specify(price ~ area) %>%
  fit()

get_confidence_interval(
  boot_fits, 
  point_estimate = observed_fit, 
  level = 0.95,
  type = "percentile"
)

get_confidence_interval(
  boot_fits, 
  point_estimate = observed_fit, 
  level = 0.95,
  type = "se"
)


get_confidence_interval(
  boot_fits, 
  point_estimate = observed_fit, 
  level = 0.95,
  type = "bias-corrected"
)


heart_edu_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(high_risk ~ education, data  = heart_disease, family = "binomial")
