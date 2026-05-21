library(ggplot2)
library(MKinfer)

herbivores <- msleep[msleep$vore == "herbi",]

n <- sum(!is.na(herbivores$sleep_total))
x <- sum(herbivores$sleep_total > 7, na.rm = TRUE)
binomCI(x, n, conf.level = 0.95, method = "wilson")

penguins %>% 
  select(species, island) %>% 
  ftable()

herbivores <- msleep[msleep$vore == "herbi",]

## t-test
penguins |>
  filter(species == "Chinstrap") |> 
  t.test(flipper_length_mm ~ sex,
         data = _)

#### t-test  one-sided
penguins |>
  filter(species == "Chinstrap") |> 
  t.test(flipper_length_mm ~ sex,
         data = _,
         alternative = "less")

## paired t test
exdata <- data.frame(before = c(8.5, 4.4, 9.4, 0.0, 2.2, 9.7, 6.2, 8.2),
                     after = c(8.5,  4.8, 10.1,  1.0,  5.6, 11.4, 7.8, 10.2))
t.test(exdata$after, exdata$before,
       paired = TRUE)

## permuted t-test
penguins |>
  filter(species == "Chinstrap") |> 
  perm.t.test(flipper_length_mm ~ sex,
              data = _)

## bootstrap t-test
penguins |>
  filter(species == "Chinstrap") |> 
  boot.t.test(flipper_length_mm ~ sex,
              data = _)

# A one-sided one-sample test with 80 % power:
power.t.test(power = 0.8, delta = 1, sd = 1, sig.level = 0.05,
             type = "one.sample", alternative = "one.sided")


## Bayesian approaches
#------------------------
# inference for a proportion
m <- stan_glm(sleep_total ~ vore, data = 
                subset(msleep, vore == "carni" | vore == "herbi"))
m
posterior_interval(m, 
                   pars = c("voreherbi"),
                   prob = 0.95)















