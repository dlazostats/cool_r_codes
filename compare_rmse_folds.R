> rf_scores
[1] 1.5141892 1.0542896 1.5234195 1.1729067 1.2659424 1.1820314 1.3708157 0.8920631 1.0647540 1.3191532
> xgb_scores
[1] 1.5681754 1.2655518 1.6813629 1.5821914 1.3185042 1.3022429 1.4768814 0.8445092 0.9897341 1.4664923

library(Hmisc)

funcv<-function(x){ sd(x)/mean(x)}

funcv(rf_scores)
funcv(xgb_scores)

GiniMd(rf_scores)
GiniMd(xgb_scores)
library(sjstats)

gmd(rf_scores)
gmd(xgb_scores)

diff <- rf_scores - xgb_scores
boxplot(diff, horizontal = TRUE, main = "RF - XGB per fold (negative = RF better)",
        xlab = "RMSE difference", col = "lightblue")
abline(v = 0, col = "red", lty = 2)

library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data
df <- data.frame(
  fold     = 1:10,
  RF       = rf_scores,
  XGB      = xgb_scores
)

df_long <- df |> pivot_longer(-fold, names_to = "model", values_to = "RMSE")

# 1. Line plot — fold-by-fold comparison (best for paired data)
ggplot(df_long, aes(x = fold, y = RMSE, color = model, group = model)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Fold RMSE: RF vs XGB", x = "Fold", y = "RMSE") +
  theme_minimal()

# 2. Boxplot — distribution comparison
ggplot(df_long, aes(x = model, y = RMSE, fill = model)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2) +
  labs(title = "RMSE Distribution by Model") +
  theme_minimal()

# 3. Difference plot — most informative for signed-rank/sign tests
df$diff <- df$RF - df$XGB

ggplot(df, aes(x = fold, y = diff)) +
  geom_col(aes(fill = diff < 0), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  labs(title = "RMSE Difference per Fold (RF - XGB)",
       subtitle = "Blue = RF better | Red = XGB better",
       x = "Fold", y = "RMSE Difference") +
  theme_minimal()
