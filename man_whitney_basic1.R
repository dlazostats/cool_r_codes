# Toy example to see the mechanics clearly
a <- c(3, 5, 7, 8, 9)     # group A (e.g. RF fold scores)
b <- c(1, 2, 4, 6, 10)    # group B (e.g. GBM fold scores)

# Step 1: combined ranks
combined <- data.frame(
  value = c(a, b),
  group = c(rep("A",5), rep("B",5))
) |> dplyr::arrange(value) |>
  dplyr::mutate(rank = 1:10)

print(combined)
#   value group rank
#       1     B    1
#       2     B    2
#       3     A    3  ← A gets rank 3
#       4     B    4
#       5     A    5
#       6     B    6
#       7     A    7
#       8     A    8
#       9     A    9
#      10     B   10

# Step 2: sum of ranks for group A
rank_sum_A <- sum(combined$rank[combined$group == "A"])  # 3+5+7+8+9 = 32
combined %>% 
  group_by(group) %>% 
  summarise(sum=sum(value))

# Step 3: U statistic
nA <- 5; nB <- 5
U_A <- rank_sum_A - (nA * (nA + 1)) / 2   # 32 - 15 = 17
U_B <- nA * nB - U_A                       # 25 - 17 = 8
U   <- min(U_A, U_B)                       # W = 8 in R's output

wilcox.test(b, a)   # confirms W = 8

# basic wilcox-test
a <- c(3, 5, 7, 8, 9)
b <- c(1, 2, 4, 6, 10)
nA <- length(a); nB <- length(b)

# Rank sum method
combined <- sort(c(a, b))
ranks_a  <- rank(c(a, b))[1:nA]   # ranks of a in the pooled set

U_A <- sum(ranks_a) - nA*(nA+1)/2
U_B <- nA*nB - U_A

cat("U_A:", U_A, "\n")   # 17  ← what R reports as W
cat("U_B:", U_B, "\n")   # 8
cat("min:", min(U_A, U_B), "\n")  # 8  ← textbook W

wilcox.test(a, b)$statistic   # 17 — confirms R uses U_A
