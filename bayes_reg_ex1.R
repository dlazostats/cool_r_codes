# ===========================================================================
# Bayesian linear regression with brms
# Same model as the from-scratch script, but sigma is now ESTIMATED, not
# assumed known — so there is no closed form and we sample instead.
#
# install.packages("brms")   # pulls in Stan; first install is slow
# ===========================================================================

library(brms)
set.seed(42)

# --- 1. Data ---------------------------------------------------------------
n      <- 50 #25
beta_t <- c(2, 0.8)      # true intercept, slope
sigma_t <- 1.0           # true noise sd — now an unknown to be recovered

d <- data.frame(x = runif(n, 0, 10))
d$y <- beta_t[1] + beta_t[2] * d$x + rnorm(n, 0, sigma_t)

plot(d$x,d$y)

# Frequentist benchmark to compare against later
ols <- lm(y ~ x, data = d)


# --- 2. What priors can I even set? ----------------------------------------
# ALWAYS run this before writing a prior. It lists every parameter, its class,
# and the default brms would use if you stay silent.
get_prior(y ~ x, data = d)
#   class = "b"          -> slopes
#   class = "Intercept"  -> intercept (of the MEAN-CENTERED model, see below)
#   class = "sigma"      -> residual sd


# --- 3. Specify priors -----------------------------------------------------
# Weakly informative: wide enough not to fight the data, tight enough to rule
# out absurdities (a slope of 10,000) and help the sampler.
priors <- c(
  prior(normal(0, 10),  class = "b"),          # slope
  prior(normal(0, 20),  class = "Intercept"),  # intercept at MEAN of x
  prior(exponential(1), class = "sigma")       # sd must be positive
)

# GOTCHA: brms centers predictors internally, so class = "Intercept" is the
# expected y at mean(x), NOT at x = 0. Usually that is what you want. To put a
# prior on the literal x = 0 intercept, suppress centering:
#     brm(y ~ 0 + Intercept + x, ...)
# and give it class = "b", coef = "Intercept".


# --- 4. Prior predictive check ---------------------------------------------
# Sample from the prior ONLY, ignoring y. Do the priors generate data that is
# even remotely plausible? Do this before you ever look at the posterior.
fit_prior <- brm(y ~ x, data = d, family = gaussian(),
                 prior = priors, sample_prior = "only",
                 chains = 2, iter = 1000, seed = 42, refresh = 0)
fit_prior
pp_check(fit_prior, ndraws = 50)   # should be wide and vague, not insane


# --- 5. Fit ----------------------------------------------------------------
fit <- brm(
  y ~ x,
  data    = d,
  family  = gaussian(),
  prior   = priors,
  chains  = 4,        # 4 independent chains
  iter    = 2000,     # 2000 each, first 1000 are warmup -> 4000 kept draws
  warmup  = 1000,
  seed    = 42,
  refresh = 0
)


# --- 6. Read the output — diagnostics FIRST --------------------------------
summary(fit)
# Look at these before anything else:
#   Rhat      must be 1.00 (1.01+ = chains disagree, results are garbage)
#   Bulk_ESS  effective sample size; want > 400, ideally > 1000
#   Tail_ESS  same, for the interval edges
# Only once those pass do the Estimate / CI columns mean anything.
#
# "l-95% CI" and "u-95% CI" are CREDIBLE intervals: given the model and data,
# there is a 95% probability the parameter lies inside. That is the statement
# people wrongly wish confint() made.

plot(fit)             # trace plots (should look like fuzzy caterpillars)
# + marginal posterior densities

fixef(fit)            # coefficient table
posterior_summary(fit)

# Side by side with OLS — with weak priors and clean data they will agree.
round(fixef(fit)[, c("Estimate", "Q2.5", "Q97.5")], 3)
round(cbind(Estimate = coef(ols), confint(ols)), 3)
cat("sigma recovered:", round(posterior_summary(fit)["sigma", "Estimate"], 3),
    " | true:", sigma_t, "\n")


# --- 7. The posterior is just a data frame ---------------------------------
# This is the real payoff: every question becomes a question about draws.
draws <- as_draws_df(fit)
head(draws[, c("b_Intercept", "b_x", "sigma")])

# P(slope > 0.5)? Count draws. No test, no p-value, no null hypothesis.
mean(draws$b_x > 0.5)

# P(slope is between 0.6 and 1.0)?
mean(draws$b_x > 0.6 & draws$b_x < 1.0)

# Posterior of any derived quantity, e.g. predicted y at x = 7:
y_at_7 <- draws$b_Intercept + draws$b_x * 7
quantile(y_at_7, c(0.025, 0.5, 0.975))

hypothesis(fit, "x > 0.5")   # brms' built-in wrapper for the same idea


# --- 8. Predictions: three different questions ------------------------------
newd <- data.frame(x = seq(0, 10, length.out = 100))

# posterior_epred  = uncertainty about the MEAN line only (narrow band)
# posterior_predict = mean uncertainty + residual noise sigma (wide band).
#                     Use this one when predicting an actual new observation.
epred <- posterior_epred(fit, newdata = newd)    # 4000 x 100
ppred <- posterior_predict(fit, newdata = newd)  # 4000 x 100

mean_band <- t(apply(epred, 2, quantile, c(.025, .5, .975)))
pred_band <- t(apply(ppred, 2, quantile, c(.025, .5, .975)))

plot(d$x, d$y, pch = 19, col = "grey30",
     ylim = range(pred_band), xlab = "x", ylab = "y",
     main = "Mean uncertainty (dark) vs predictive uncertainty (light)")
polygon(c(newd$x, rev(newd$x)), c(pred_band[,1], rev(pred_band[,3])),
        col = rgb(1, 0, 0, 0.10), border = NA)
polygon(c(newd$x, rev(newd$x)), c(mean_band[,1], rev(mean_band[,3])),
        col = rgb(1, 0, 0, 0.30), border = NA)
lines(newd$x, mean_band[, 2], col = "red", lwd = 2)
points(d$x, d$y, pch = 19, col = "grey30")

conditional_effects(fit)   # the one-liner version of the above


# --- 9. Posterior predictive check -----------------------------------------
# The single most valuable habit in applied Bayes. Simulate datasets from the
# fitted model and ask: do they look like the data I actually observed?
# If the blue replicates miss the shape of the dark observed line, the model
# is wrong in a way no coefficient table will tell you.
pp_check(fit, ndraws = 100)
pp_check(fit, type = "scatter_avg")
pp_check(fit, type = "stat", stat = "sd")   # does it reproduce the spread?


# --- 10. Does the prior matter? --------------------------------------------
# A stubborn prior insisting the slope is near zero. With n = 25 the data
# cannot fully overrule it; the estimate gets dragged toward 0 (this is ridge
# regression). Refit with n = 10000 and the likelihood would win easily.
fit_tight <- update(fit, prior = c(
  prior(normal(0, 0.05), class = "b"),
  prior(normal(0, 20),   class = "Intercept"),
  prior(exponential(1),  class = "sigma")
), refresh = 0)

cat("\nSlope under weak prior: ", round(fixef(fit)["x", "Estimate"], 3), "\n")
cat("Slope under tight prior:", round(fixef(fit_tight)["x", "Estimate"], 3), "\n")
cat("True slope:             ", beta_t[2], "\n")


# --- 11. Model comparison --------------------------------------------------
# LOO = leave-one-out cross-validation, estimated from the posterior draws.
# elpd_diff more than ~2x its se_diff is a meaningful difference.
fit_null <- brm(y ~ 1, data = d, family = gaussian(), chains = 4,
                iter = 2000, seed = 42, refresh = 0)
loo_compare(loo(fit), loo(fit_null))


# ===========================================================================
# WHERE brms EARNS ITS KEEP
# The formula syntax extends far past lm(), and none of it changes the
# workflow above — you still set priors, check Rhat, and pp_check:
#
#   y ~ x + (1 + x | group)          varying intercepts and slopes (multilevel)
#   bf(y ~ x, sigma ~ x)             heteroskedasticity: model sigma itself
#   family = student()               robust regression, heavy tails
#   family = bernoulli()             logistic regression
#   family = negbinomial()           overdispersed counts
#   y | cens(censored) ~ x           censored outcomes
#   y ~ s(x)                         splines / GAMs
#   y ~ me(x, x_se)                  measurement error in a predictor
# ===========================================================================