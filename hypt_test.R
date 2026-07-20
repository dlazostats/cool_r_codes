# Example 1
#-------------------------------------------------------------------------------------------------------
dta<-data.frame(hours=c(31.94, 140.44, 39.12, 82.47, 20.64, 37.89, 1.14, 15.4, 121.91, 15.59))
mean(dta$hours)
sd(dta$hours)
dta %>%
  t_test(response = hours, mu = 72)

# using bootstraping
# using t
observed <- dta %>%
            specify(response = hours) %>%
            hypothesize(null = "point", mu = 70) %>%
            calculate(stat = "t")
null_dist <- dta %>%
            specify(response = hours) %>%
            hypothesize(null = "point", mu = 70) %>%
            generate(reps = 1000, type = "bootstrap") %>%
            calculate(stat = "t")
null_dist %>%
  get_p_value(obs_stat = observed, direction = "two-sided")
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = observed, direction = "two-sided")

# using mean
observed <- dta %>%
  specify(response = hours) %>%
  calculate(stat = "mean")
null_dist <- dta %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 70) %>%   # mu matters here
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
null_dist %>%
  get_p_value(obs_stat = observed, direction = "two-sided")
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = observed, direction = "two-sided")

# theory based 
observed <- dta %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 70) %>%
  calculate(stat = "t")
null_dist <- dta %>%
  specify(response = hours) %>%
  assume(distribution = "t")
null_dist %>%
  get_p_value(obs_stat = observed, direction = "two-sided")

# Example 2
#-------------------------------------------------------------------------------------------------------
set.seed(1)
control   <- c(4.1, 5.2, 3.8, 4.9, 5.1, 4.3)
treatment <- c(5.8, 6.1, 5.5, 6.4, 5.9, 6.2)
observed <- mean(treatment) - mean(control)

combined <- c(control, treatment)
n_control <- length(control)
n_perm <- 10000
perm_stats <- numeric(n_perm)
for (i in seq_len(n_perm)) {
  shuffled <- sample(combined)
  g1 <- shuffled[1:n_control]
  g2 <- shuffled[(n_control + 1):length(combined)]
  perm_stats[i] <- mean(g2) - mean(g1)
}
p_value <- mean(abs(perm_stats) >= abs(observed))
p_value


## power
set.seed(5)
n_sim <- 10000
n <- 20
true_effect <- 0.8   # real difference in means
reject <- logical(n_sim)
for (i in seq_len(n_sim)) {
  a <- rnorm(n, mean = 0)
  b <- rnorm(n, mean = true_effect)
  reject[i] <- t.test(a, b)$p.value < 0.05
}
power <- mean(reject)   # fraction of times we detect the real effect
power
