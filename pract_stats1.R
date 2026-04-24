m1<-lm(mpg~disp+hp+drat,data=mtcars)
s1<-summary(m1)
s1$sigma
s1$r.squared
plot(m1)

library(tidyverse)
library(rsample)
mtcars %>% 
  group_by(as.factor(cyl)) %>% 
  summarise(m=mean(mpg),
            s=sd(mpg),
            n=n(),
            lo=m-qt(.975,n-1)*s/sqrt(n), hi=m+qt(.975,n-1)*s/sqrt(n))

#using bootstrapping
mtcars %>%
  select(mpg, cyl) %>%
  nest(data = -cyl) %>%
  mutate(
    boot  = map(data, ~ bootstraps(.x, times = 1000)),
    stats = map(boot, ~ {
      means <- map_dbl(.x$splits, ~ mean(analysis(.x)$mpg))  # ← mpg, not $value
      tibble(
        mean     = mean(means),
        sd       = sd(analysis(.x$splits[[1]])$mpg),          # ← mpg, not $value
        ci_lower = quantile(means, 0.025),
        ci_upper = quantile(means, 0.975)
      )
    })
  ) %>%
  unnest(stats) %>%
  select(cyl, mean, sd, ci_lower, ci_upper)                   # ← cyl, not group

#using jackkinfe
mtcars %>%
  select(mpg, cyl) %>%
  nest(data = -cyl) %>%
  mutate(
    stats = map(data, ~ {
      x     <- .x$mpg
      n     <- length(x)
      
      # Leave-one-out means
      jack_means <- map_dbl(1:n, ~ mean(x[-.x]))
      
      # Jackknife estimates
      jack_mean  <- mean(jack_means)
      jack_se    <- sqrt(((n - 1) / n) * sum((jack_means - jack_mean)^2))
      
      tibble(
        mean     = mean(x),                          # original mean
        sd       = sd(x),                            # original SD
        ci_lower = jack_mean - 1.96 * jack_se,
        ci_upper = jack_mean + 1.96 * jack_se
      )
    })
  ) %>%
  unnest(stats) %>%
  select(cyl, mean, sd, ci_lower, ci_upper)

# for the sd
mtcars %>%
  select(mpg, cyl) %>%
  nest(data = -cyl) %>%
  mutate(
    stats = map(data, ~ {
      x <- .x$mpg
      n <- length(x)
      
      # Leave-one-out means and SDs
      jack_means <- map_dbl(1:n, ~ mean(x[-.x]))
      jack_sds   <- map_dbl(1:n, ~ sd(x[-.x]))
      
      # Jackknife SEs
      jack_mean_se <- sqrt(((n - 1) / n) * sum((jack_means - mean(jack_means))^2))
      jack_sd_se   <- sqrt(((n - 1) / n) * sum((jack_sds   - mean(jack_sds))^2))
      
      tibble(
        mean        = mean(x),
        ci_lower_mean = mean(jack_means) - 1.96 * jack_mean_se,
        ci_upper_mean = mean(jack_means) + 1.96 * jack_mean_se,
        sd          = sd(x),
        ci_lower_sd = mean(jack_sds) - 1.96 * jack_sd_se,
        ci_upper_sd = mean(jack_sds) + 1.96 * jack_sd_se
      )
    })
  ) %>%
  unnest(stats) %>%
  select(cyl, mean, ci_lower_mean, ci_upper_mean, sd, ci_lower_sd, ci_upper_sd)

# for the sd - bootstrap
mtcars %>%
  select(mpg, cyl) %>%
  nest(data = -cyl) %>%
  mutate(
    boot  = map(data, ~ bootstraps(.x, times = 1000)),
    stats = map2(data, boot, ~ {                              # ← map2 to access both
      x          <- .x$mpg                                   # ← original data from data column
      boot_means <- map_dbl(.y$splits, ~ mean(analysis(.x)$mpg))  
      boot_sds   <- map_dbl(.y$splits, ~ sd(analysis(.x)$mpg))    
      
      tibble(
        mean          = mean(x),                             # ← original mean
        ci_lower_mean = quantile(boot_means, 0.025),
        ci_upper_mean = quantile(boot_means, 0.975),
        sd            = sd(x),                              # ← original sd
        ci_lower_sd   = quantile(boot_sds, 0.025),
        ci_upper_sd   = quantile(boot_sds, 0.975)
      )
    })
  ) %>%
  unnest(stats) %>%
  select(cyl, mean, ci_lower_mean, ci_upper_mean, sd, ci_lower_sd, ci_upper_sd)






















