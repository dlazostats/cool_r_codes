library(microbenchmark)
library(ggplot2)
n <- 1000
mb <- microbenchmark(
  seq(1, n, 1),
  seq.int(1, n, 1),
  1:n,
)
mb
autoplot(mb)