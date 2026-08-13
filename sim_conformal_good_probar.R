# =====================================================================
# conformal_sim.R
# Simulate data to stress-test conformal prediction methods, with a
# focus on LOCAL / heterogeneous noise behaviour.
#
# Design principle: every scenario has a KNOWN conditional distribution,
# so we can return oracle prediction intervals. That lets you measure
# what actually distinguishes conformal methods -- conditional coverage
# and how interval width tracks the true local noise -- instead of just
# marginal coverage (which any valid method attains).
#
# Base R only (uses splines::ns, which ships with R).
# =====================================================================


# ---------------------------------------------------------------------
# 1. DATA SIMULATOR
# ---------------------------------------------------------------------
# Returns a data.frame with:
#   x (or x1..xp), y            -- the observed data
#   mu_true                     -- true conditional mean
#   sigma_true                  -- true conditional scale (local noise)
#   oracle_lo, oracle_hi        -- exact conditional (1 - alpha) interval
#   group                       -- only for scenario "regimes"
#
# Scenarios (what each one stresses):
#   "smooth_hetero" -- variance rises monotonically with x. Baseline
#                      heteroscedasticity; constant-width intervals
#                      over-cover on the left, under-cover on the right.
#   "local_pockets" -- two localized bursts of high variance on an
#                      otherwise quiet function. THE local-heterogeneity
#                      case: methods must adapt width sharply and locally.
#   "skewed"        -- right-skewed (gamma) noise. Symmetric residual
#                      intervals are miscalibrated per-side; separates
#                      residual conformal from CQR-style quantile methods.
#   "regimes"       -- discrete groups with different mean & variance.
#                      Tests group-conditional coverage (Mondrian).
#   "multivar"      -- p features; noise driven by a couple of them.
#                      Tests whether a method finds the relevant local
#                      structure among nuisance features.
# ---------------------------------------------------------------------

sim_conformal_data <- function(n = 2000,
                               scenario = c("smooth_hetero",
                                            "local_pockets",
                                            "skewed",
                                            "regimes",
                                            "multivar"),
                               alpha = 0.10,
                               p = 5,          # only used by "multivar"
                               seed = NULL) {
  
  scenario <- match.arg(scenario)
  if (!is.null(seed)) set.seed(seed)
  
  z_lo <- qnorm(alpha / 2)          # negative
  z_hi <- qnorm(1 - alpha / 2)      # positive
  
  if (scenario == "smooth_hetero") {
    x         <- runif(n, 0, 5)
    mu        <- 2 * sin(1.5 * x)
    sigma     <- 0.2 + 0.35 * x                 # grows with x
    eps       <- rnorm(n)
    y         <- mu + sigma * eps
    oracle_lo <- mu + sigma * z_lo
    oracle_hi <- mu + sigma * z_hi
    out <- data.frame(x, y, mu_true = mu, sigma_true = sigma,
                      oracle_lo, oracle_hi)
    
  } else if (scenario == "local_pockets") {
    x     <- runif(n, 0, 10)
    mu    <- 0.5 * x + sin(x)
    # quiet baseline + two Gaussian "bumps" of high local variance
    sigma <- 0.3 +
      2.0 * exp(-((x - 3)^2) / (2 * 0.6^2)) +
      1.5 * exp(-((x - 7)^2) / (2 * 0.8^2))
    eps       <- rnorm(n)
    y         <- mu + sigma * eps
    oracle_lo <- mu + sigma * z_lo
    oracle_hi <- mu + sigma * z_hi
    out <- data.frame(x, y, mu_true = mu, sigma_true = sigma,
                      oracle_lo, oracle_hi)
    
  } else if (scenario == "skewed") {
    x     <- runif(n, 0, 5)
    mu    <- 1 + 0.8 * x
    sigma <- 0.3 + 0.4 * x                       # scale also grows
    # centered, unit-variance right-skewed noise (gamma shape 2)
    shp   <- 2
    g     <- rgamma(n, shape = shp, rate = 1)
    eps   <- (g - shp) / sqrt(shp)               # mean 0, var 1, skewed
    y     <- mu + sigma * eps
    # oracle uses the ASYMMETRIC gamma quantiles
    ql        <- (qgamma(alpha / 2,     shp, 1) - shp) / sqrt(shp)
    qh        <- (qgamma(1 - alpha / 2, shp, 1) - shp) / sqrt(shp)
    oracle_lo <- mu + sigma * ql
    oracle_hi <- mu + sigma * qh
    out <- data.frame(x, y, mu_true = mu, sigma_true = sigma,
                      oracle_lo, oracle_hi)
    
  } else if (scenario == "regimes") {
    group <- sample(1:3, n, replace = TRUE)
    x     <- runif(n, 0, 5)
    mu    <- c(0, 2, 4)[group] + 0.5 * x
    sigma <- c(0.3, 1.0, 2.5)[group]            # variance differs by group
    eps       <- rnorm(n)
    y         <- mu + sigma * eps
    oracle_lo <- mu + sigma * z_lo
    oracle_hi <- mu + sigma * z_hi
    out <- data.frame(x, y, group = factor(group),
                      mu_true = mu, sigma_true = sigma,
                      oracle_lo, oracle_hi)
    
  } else if (scenario == "multivar") {
    X     <- matrix(rnorm(n * p), n, p)
    beta  <- c(1.5, -1.0, rep(0, p - 2))        # only first two matter for mean
    mu    <- as.vector(X %*% beta)
    # local noise driven by features 1 and 3 only (nonlinear + interaction)
    sigma <- 0.4 + exp(0.5 * X[, 1]) * (0.3 + 0.7 * (X[, 3] > 0))
    eps       <- rnorm(n)
    y         <- mu + sigma * eps
    oracle_lo <- mu + sigma * z_lo
    oracle_hi <- mu + sigma * z_hi
    out <- data.frame(X)
    names(out) <- paste0("x", 1:p)
    out$y          <- y
    out$mu_true    <- mu
    out$sigma_true <- sigma
    out$oracle_lo  <- oracle_lo
    out$oracle_hi  <- oracle_hi
  }
  
  attr(out, "scenario") <- scenario
  attr(out, "alpha")    <- alpha
  out
}


# ---------------------------------------------------------------------
# 2. EVALUATION HELPERS
# ---------------------------------------------------------------------
# Marginal coverage is necessary but boring. The informative metrics:
#   - conditional coverage across bins of x (or across groups)
#   - mean interval width
#   - adaptivity: correlation between interval width and true local sigma
#     (an oracle-aware method should have width strongly correlated with
#      sigma_true; a constant-width method has correlation 0)
# ---------------------------------------------------------------------

eval_intervals <- function(y, lo, hi,
                           sigma_true = NULL,
                           x = NULL, n_bins = 10,
                           group = NULL) {
  
  covered <- (y >= lo) & (y <= hi)
  width   <- hi - lo
  
  res <- list(
    marginal_coverage = mean(covered),
    mean_width        = mean(width),
    median_width      = median(width)
  )
  
  if (!is.null(sigma_true)) {
    # how well width tracks the true local scale
    res$width_sigma_cor <- suppressWarnings(cor(width, sigma_true))
  }
  
  # conditional coverage by bins of a 1-D x
  if (!is.null(x)) {
    bins <- cut(x, breaks = quantile(x, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    res$conditional <- data.frame(
      bin        = levels(bins),
      coverage   = tapply(covered, bins, mean),
      mean_width = tapply(width,   bins, mean),
      row.names  = NULL
    )
  }
  
  # conditional coverage by group (Mondrian setting)
  if (!is.null(group)) {
    res$by_group <- data.frame(
      group      = levels(factor(group)),
      coverage   = tapply(covered, group, mean),
      mean_width = tapply(width,   group, mean),
      row.names  = NULL
    )
  }
  
  res
}


# ---------------------------------------------------------------------
# 3. SPLIT CONFORMAL: constant-width vs. locally-adaptive (normalized)
# ---------------------------------------------------------------------
# Both are valid (marginal coverage guaranteed). The point of the
# comparison is LOCAL behaviour: constant width fails under
# heterogeneity; the normalized version adapts by dividing residuals
# by an estimated local scale sigma_hat(x).
#
# These helpers are written for the univariate scenarios. For
# "multivar", swap the ns() mean/scale models for ranger / grf, or use
# conformalized quantile regression (quantreg::rq or grf::quantile_forest).
# ---------------------------------------------------------------------

# conformal quantile level with the finite-sample correction
.conf_q <- function(scores, alpha) {
  m     <- length(scores)
  level <- ceiling((m + 1) * (1 - alpha)) / m
  if (level >= 1) return(Inf)          # not enough calibration points
  as.numeric(quantile(scores, probs = level, type = 1))
}

split_conformal <- function(df, alpha = 0.10,
                            normalized = FALSE,
                            df_spline = 8,
                            prop_train = 0.5) {
  
  n   <- nrow(df)
  idx <- sample(n)
  n_tr <- floor(prop_train * n)
  tr  <- idx[1:n_tr]                    # fit models here
  cal <- idx[(n_tr + 1):n]             # calibrate scores here
  
  # --- mean model ---
  mu_fit  <- lm(y ~ splines::ns(x, df = df_spline), data = df[tr, ])
  mu_hat  <- function(newx) predict(mu_fit, newdata = data.frame(x = newx))
  
  resid_cal <- df$y[cal] - mu_hat(df$x[cal])
  
  if (!normalized) {
    scores <- abs(resid_cal)
    q      <- .conf_q(scores, alpha)
    lo     <- mu_hat(df$x) - q
    hi     <- mu_hat(df$x) + q
  } else {
    # --- local scale model: regress |residual| on x ---
    resid_tr <- abs(df$y[tr] - mu_hat(df$x[tr]))
    sig_fit  <- lm(resid_tr ~ splines::ns(df$x[tr], df = df_spline))
    sig_hat  <- function(newx) {
      s <- predict(sig_fit,
                   newdata = data.frame(`splines::ns(df$x[tr], df = df_spline)` = NA))
      # predict() above is awkward with ns in formula; refit cleanly:
      NA
    }
    # cleaner refit so prediction is straightforward:
    d_sig   <- data.frame(x = df$x[tr], r = resid_tr)
    sig_fit <- lm(r ~ splines::ns(x, df = df_spline), data = d_sig)
    sig_hat <- function(newx) {
      s <- predict(sig_fit, newdata = data.frame(x = newx))
      pmax(s, 1e-3)                     # floor to stay positive
    }
    
    scores <- abs(resid_cal) / sig_hat(df$x[cal])
    q      <- .conf_q(scores, alpha)
    lo     <- mu_hat(df$x) - q * sig_hat(df$x)
    hi     <- mu_hat(df$x) + q * sig_hat(df$x)
  }
  
  data.frame(lo, hi)
}


# ---------------------------------------------------------------------
# 4. DEMO
# ---------------------------------------------------------------------
run_demo <- function(scenario = "local_pockets", alpha = 0.10, seed = 1) {
  
  df <- sim_conformal_data(n = 3000, scenario = scenario,
                           alpha = alpha, seed = seed)
  
  # Oracle (best achievable): exact conditional interval
  oracle_eval <- eval_intervals(df$y, df$oracle_lo, df$oracle_hi,
                                sigma_true = df$sigma_true, x = df$x)
  
  set.seed(seed)
  const_iv  <- split_conformal(df, alpha = alpha, normalized = FALSE)
  set.seed(seed)
  adapt_iv  <- split_conformal(df, alpha = alpha, normalized = TRUE)
  
  const_eval <- eval_intervals(df$y, const_iv$lo, const_iv$hi,
                               sigma_true = df$sigma_true, x = df$x)
  adapt_eval <- eval_intervals(df$y, adapt_iv$lo, adapt_iv$hi,
                               sigma_true = df$sigma_true, x = df$x)
  
  cat(sprintf("\n=== scenario: %s  (target coverage %.2f) ===\n",
              scenario, 1 - alpha))
  summ <- function(tag, e) cat(sprintf(
    "%-14s | marg.cov %.3f | mean width %.2f | width~sigma cor %s\n",
    tag, e$marginal_coverage, e$mean_width,
    ifelse(is.null(e$width_sigma_cor), "  NA",
           sprintf("%.2f", e$width_sigma_cor))))
  
  summ("oracle",        oracle_eval)
  summ("const width",   const_eval)
  summ("normalized",    adapt_eval)
  
  cat("\nConditional coverage by x-bin (should all be ~", 1 - alpha, "):\n", sep = "")
  cmp <- data.frame(
    bin              = const_eval$conditional$bin,
    cov_const        = round(const_eval$conditional$coverage, 3),
    cov_normalized   = round(adapt_eval$conditional$coverage, 3),
    width_const      = round(const_eval$conditional$mean_width, 2),
    width_normalized = round(adapt_eval$conditional$mean_width, 2)
  )
  print(cmp, row.names = FALSE)
  
  invisible(list(df = df, const = const_eval, adapt = adapt_eval,
                 oracle = oracle_eval))
}

# To run:
#   source("conformal_sim.R")
#   run_demo("local_pockets")
#   run_demo("smooth_hetero")
#   run_demo("skewed")        # watch per-bin coverage; symmetric intervals wobble
#
# For "regimes":  eval_intervals(..., group = df$group) for Mondrian coverage.
# For "multivar": replace ns() models with ranger/grf, or use CQR.


# To run:
source("conformal_sim.R")
run_demo("local_pockets")
run_demo("smooth_hetero")
run_demo("skewed")    












