library(dirichletprocess)
library(ggplot2)

set.seed(123)

x <- c(
  rnorm(100, -3, 0.5),
  rnorm(100, 0, 0.4),
  rnorm(100, 3, 0.6)
)

df <- data.frame(x)

ggplot(df, aes(x)) +
  geom_histogram(bins = 40, fill="steelblue") +
  theme_minimal()

dp <- DirichletProcessGaussian(x)
#fit MCMC
dp <- Fit(dp, 1000)

length(unique(dp$clusterLabels))
plot(dp)

df$cluster <- as.factor(dp$clusterLabels)
ggplot(df, aes(x, fill=cluster)) +
  geom_histogram(bins=40, alpha=0.6, position="identity") +
  theme_minimal()

table(df$cluster)


df$id_or<-c(rep(1,100),rep(2,100),rep(3,100))
table(df$cluster,df$id_or)


# con 4 categorias?
set.seed(123)

x <- c(
  rnorm(100, -3, 0.5),
  rnorm(100, 0, 0.4),
  rnorm(100, 3, 0.6),
  rnorm(100, 6, 0.3)
)

df <- data.frame(x)
ggplot(df, aes(x)) +
  geom_histogram(bins = 40, fill="steelblue") +
  theme_minimal()

dp <- DirichletProcessGaussian(x)
dp <- Fit(dp, 1000)#fit MCMC

length(unique(dp$clusterLabels))
plot(dp)

df$cluster <- as.factor(dp$clusterLabels)
ggplot(df, aes(x, fill=cluster)) +
  geom_histogram(bins=40, alpha=0.6, position="identity") +
  theme_minimal()

table(df$cluster)


df$id_or<-c(rep(1,100),rep(2,100),rep(3,100),rep(4,100))
table(df$cluster,df$id_or)

