# --- Robust BCa density -------------------------------------------------------
bca_density <- function(t_boot, t0, t_jack,
                        bw = "nrd0", adjust = 1,
                        from = NULL, to = NULL, n = 512,
                        plot = TRUE) {
  # basic checks + cleaning
  stopifnot(is.numeric(t_boot), is.numeric(t0), length(t0) == 1L,
            is.numeric(t_jack))
  t_boot <- t_boot[is.finite(t_boot)]
  t_jack <- t_jack[is.finite(t_jack)]
  if (length(t_boot) < 30L) stop("Need at least ~30 bootstrap draws.")
  if (length(t_jack) < 3L)  stop("Need at least 3 jackknife values.")
  
  # bias-correction z0 (mid-p for ties)
  eps <- 1e-8
  u0 <- mean(t_boot < t0) + 0.5 * mean(t_boot == t0)
  u0 <- min(max(u0, eps), 1 - eps)
  z0 <- qnorm(u0)
  
  # acceleration a from jackknife
  tbar_j <- mean(t_jack)
  num <- sum((tbar_j - t_jack)^3)
  den <- 6 * (sum((tbar_j - t_jack)^2))^(3/2)
  a <- if (den == 0) 0 else num / den  # guard perfectly flat stats
  
  # density() wrapper: only pass from/to when non-NULL scalars
  args <- list(x = t_boot, bw = bw, adjust = adjust, n = n)
  if (!is.null(from)) {
    stopifnot(is.numeric(from), length(from) == 1L, is.finite(from))
    args$from <- from
  }
  if (!is.null(to)) {
    stopifnot(is.numeric(to), length(to) == 1L, is.finite(to))
    args$to <- to
  }
  kd <- do.call(density, args)
  
  grid <- kd$x
  f_star <- kd$y
  
  # smooth CDF using trapezoid on the same grid (keeps bw consistent with pdf)
  dx <- diff(grid)
  fmid <- (f_star[-1] + f_star[-length(f_star)]) / 2
  F_star <- c(0, cumsum(fmid * dx))
  F_star <- F_star / max(F_star)                   # normalize
  F_star <- pmin(pmax(F_star, eps), 1 - eps)       # clamp
  
  # derivative of BCa alpha(u)
  z <- qnorm(F_star)
  denom <- (1 - a * (z0 + z))
  # avoid division by ~0 in extreme tails
  denom[abs(denom) < 1e-10] <- sign(denom[abs(denom) < 1e-10]) * 1e-10
  W <- z0 + (z0 + z) / denom
  phi <- dnorm
  alpha_prime <- phi(W) / (phi(z) * denom^2)
  
  # BCa-adjusted density
  g <- f_star * alpha_prime
  
  # optional plot
  if (isTRUE(plot)) {
    oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
    plot(grid, f_star, type = "l", xlab = "t", ylab = "Density",
         main = "Bootstrap vs BCa-adjusted density")
    lines(grid, g, lty = 2)
    legend("topright", c("Bootstrap kernel", "BCa-adjusted"),
           lty = c(1, 2), bty = "n")
  }
  
  # interpolators
  dens_boot_fun <- approxfun(grid, f_star, yleft = 0, yright = 0)
  dens_bca_fun  <- approxfun(grid, g,       yleft = 0, yright = 0)
  
  list(
    x = grid,
    f_boot = f_star,
    F_boot = F_star,
    density_bca = g,
    dens_fun_boot = dens_boot_fun,
    dens_fun_bca  = dens_bca_fun,
    z0 = z0, a = a,
    call = match.call()
  )
}

set.seed(1)
x <- rlnorm(80, 0, 0.8)
theta <- mean(x)
B <- 3000
t_boot <- replicate(B, mean(sample(x, replace = TRUE)))
t_jack <- sapply(seq_along(x), function(i) mean(x[-i]))

res <- bca_density(t_boot, t0 = theta, t_jack = t_jack, plot = TRUE)

res <- bca_density(t_boot, t0 = theta, t_jack = t_jack,
                   from = quantile(t_boot, 0.001),
                   to   = quantile(t_boot, 0.999))
