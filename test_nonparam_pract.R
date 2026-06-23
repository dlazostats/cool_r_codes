library(effectsize)

# ranked based test
# Mann-Whitney U
group_a <- c(45, 52, 37, 61, 48, 55, 42, 38, 66, 51)
group_b <- c(29, 33, 41, 27, 35, 22, 39, 30, 44, 28)

# 1. Perform the test (continuity correction off for small n)
wilcox.test(group_a, group_b, correct = FALSE)

# 2. Effect size: rank-biserial correlation
# install.packages("effectsize")

rank_biserial(group_a, group_b)

#Concretely, r = 0.86 means: if you picked one random value from each group, group_a 
#would be larger ~93% of the time (calculated as (1 + r) / 2).


## Paired data
pre  <- c(450, 380, 510, 420, 600, 390, 470, 445, 520, 415, 490, 560)
post <- c(310, 290, 400, 380, 520, 300, 410, 390, 460, 350, 420, 490)

# Test the null that the median difference is zero
wilcox.test(pre, post, paired = TRUE)

# Check distribution of differences
diff <- pre - post
hist(diff, breaks = 8)

##  k independent groups
yields <- data.frame(
  yield = c(22,25,24,27,23,30,35,31,33,28,18,20,17,19,21),
  trt   = rep(c("A","B","C"), each = 5)
)

# Omnibus test
kruskal.test(yield ~ trt, data = yields)

# Post-hoc (install.packages("dunn.test"))
library(dunn.test)
dunn.test(yields$yield, yields$trt, method = "bonferroni")
