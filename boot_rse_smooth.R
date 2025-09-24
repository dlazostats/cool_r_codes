# Residual Standar Error from Bootstrap
#--------------------------------------
library(dplyr)
library(rsample)
library(tidymodels)
library(ggplot2)

# Natural Spline
#----------------
# create a data fame
set.seed(0)
n = 500
x = sort(runif(n,0,6*pi))
r = x*sin(x) 
y = r + rnorm(n)
plot(x,y,col="gray50")
lines(x,r,col="blue",lwd=2)
dta<-data.frame(x=x,y=y)

# define Bootstrap and model
set.seed(123) 
boot <- bootstraps(dta, times = 1000)

lm_spec <- linear_reg() %>%
           set_engine("lm")
spline_recipe <- recipe(y ~ x, data = dta) %>%
                 step_ns(x, deg_free = 25) 
spline_wf <- workflow() %>%
             add_model(lm_spec) %>%
             add_recipe(spline_recipe)

fit_spline_on_bootstrap <- function(split) {
  model_fit <- fit(spline_wf, data = analysis(split))
  glance(model_fit) %>%
    pull(sigma)
}

res_boot <- boot %>%
            mutate(rse = map_dbl(splits, fit_spline_on_bootstrap))
quantile(res_boot$rse, c(0.025, 0.975))

ggplot(res_boot, aes(x = rse)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Residual Standard Error from Bootstrap Samples",
    x = "Residual Standard Error (RSE)",
    y = "Frequency"
  ) +
  theme_minimal()

# single value
mdl_ns<-fit(spline_wf,data=dta)
glance(mdl_ns)
glance(mdl_ns) %>% pull(sigma)

#using a linear model
lm_recipe <- recipe(mpg ~ disp+drat+wt, data = mtcars) 
lm_wf <- workflow() %>%
         add_model(lm_spec) %>%
         add_recipe(lm_recipe)
lm_fit<-fit(lm_wf,data=mtcars)
glance(lm_fit)

boot <- bootstraps(mtcars, times = 1000)
fit_lm_on_bootstrap <- function(split) {
  model_fit <- fit(lm_wf, data = analysis(split))
  glance(model_fit) %>%
    pull(sigma)
}

res_boot <- boot %>%
            mutate(rse = map_dbl(splits, fit_lm_on_bootstrap))
quantile(res_boot$rse, c(0.025, 0.50,0.975))
