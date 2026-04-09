# infer
#---------
library(ggplot2)
library(infer)
library(DescTools)
data(gss)
head(gss)
dplyr::glimpse(gss)

# set objective
hist(gss$age,breaks="FD")
gss |>
  specify(response = age)
gss |>
  specify(age ~ partyid)
ggplot(gss, aes(x = age, y = after_stat(density))) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~partyid)

gss |>
  specify(response = age, explanatory = partyid)

# specifying for inference on proportions
gss |>
  specify(response = college, success = "degree")

# hipotesis
#------------
Desc(gss$hours)
gss |>
  specify(college ~ partyid, success = "degree") |>
  hypothesize(null = "independence")

# using bootstraping
set.seed(2121)
gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40)|>
  generate(reps = 1000, type = "bootstrap")->hyboot1

hyboot1 %>% 
  group_by(replicate) %>% 
  summarise(prom=mean(hours))->dt_hyboot
hist(dt_hyboot$prom,breaks="FD")

obs_mean<-mean(gss$hours)
p_value <- mean(abs(dt_hyboot$prom - 40) >= abs(obs_mean - 40))
p_value

gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "mean") %>% 
  visualize() +
  shade_p_value(obs_stat = obs_mean, direction = "two-sided")

null_dist <-gss |>
            specify(response = hours) |>
            hypothesize(null = "point", mu = 40) |>
            generate(reps = 1000, type = "bootstrap") |>
            calculate(stat = "mean")

null_dist|>
  get_p_value(obs_stat = obs_mean, direction = "two-sided")

boot_dist <- gss |>
  specify(response = hours) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "mean")
ci <- boot_dist |>
  get_confidence_interval(
    point_estimate = obs_mean,
    level = .95,
    type = "se"
  )
ci

# independencia 
gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate("diff in means", order = c("degree", "no degree"))

boot_dist |>
  visualize() +
  shade_confidence_interval(endpoints = ci)

#Theoretical methods
#--------------------
obs_t <- gss |>
         specify(response = hours) |>
         hypothesize(null = "point", mu = 40) |>
         calculate(stat = "t")
t_dist <- gss |>
          specify(response = hours) |>
          assume(distribution = "t")
visualize(t_dist) +
  shade_p_value(obs_stat = obs_t, direction = "greater") 
get_p_value(t_dist, obs_t, "greater")

point_estimate <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

theor_ci <- get_confidence_interval(
    x = t_dist,
    level = .95,
    point_estimate = point_estimate
  )

visualize(t_dist) +
  shade_confidence_interval(theor_ci)

# multiple regression
#--------------------
observed_fit <- gss |>
                specify(hours ~ age + college) |>
                fit()
coef(lm(hours ~ age + college,data=gss))

null_fits <- gss |>
            specify(hours ~ age + college) |>
            hypothesize(null = "independence") |>
            generate(reps = 1000, type = "permute") |> #To permute variables other than the response variable
            fit()

null_fits

get_confidence_interval(
  null_fits, 
  point_estimate = observed_fit, 
  level = .95
)

confint(lm(hours ~ age + college,data=gss))

visualize(null_fits) + 
  shade_p_value(observed_fit, direction = "both")

summary(lm(hours ~ age + college,data=gss))


null_fits_boot <- gss |>
  specify(hours ~ age + college) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "bootstrap") |> #To permute variables other than the response variable
  fit()

get_confidence_interval(
  null_fits_boot, 
  point_estimate = observed_fit, 
  level = .95
)

visualize(null_fits_boot) + 
  shade_p_value(observed_fit, direction = "both")













