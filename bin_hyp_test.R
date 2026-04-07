#--one-proportion Z-test in R---
# number of successes
x<-28

# sample size
n<-31

#Hypothesized proportion under null hypothesis
p0<-0.9

# Perform the test
# We set `correct = FALSE` to match the standard z-test formula.
# The default `alternative` is "two.sided", which is what we need
prop_test_result <- prop.test(x = x, n = n, p = p0, correct = FALSE)
prop_test_result

# --- Exact Binomial Test in R ---
# Perform the test
# We specify `alternative = "less"` for a one-sided test.
binom_test_result <- binom.test(x = x, n = n, p = p0, alternative = "less")
binom_test_result
