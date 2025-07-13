library(Lock5Data)
data("CommuteAtlanta")
str(CommuteAtlanta)
library(boot)
my.mean = function(x, indices) {
  return( mean( x[indices] ) )
}
tmboot<-boot(CommuteAtlanta$Time,my.mean,1000)
ggplot()
mean(CommuteAtlanta$Time)
boot.ci(tmboot)
ggplot(data.frame(x=tmboot$t),aes(x=x)) +
  geom_histogram(aes(y=..density..)) + geom_density(color="red")

## for linear regresssion
model_coef <- function(data, index){
  coef(lm(Fertility ~., data = data, subset = index))
}
model_coef(swiss, 1:47)
library(boot)
cibot<-boot(swiss, model_coef, 500)
summary(lm(Fertility ~., data = swiss))
cibot


# Example 2
dlsat<-data.frame(LSAT =c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594),
                  GPA =c(339, 330, 281, 303, 344, 307, 300, 343, 336, 313, 312, 274, 276, 288, 296))
library(bootstrap) #for the law data
print(cor(law$LSAT, law$GPA)) #cor(dlsat$LSAT,dlsat$GPA)
print(cor(law82$LSAT, law82$GPA))

#set up the bootstrap
B <- 1000 #number of replicates
n <- nrow(law) #sample size
R <- numeric(B) #storage for replicates
#bootstrap estimate of standard error of R
for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT <- law$LSAT[i] #i is a vector of indices
  GPA <- law$GPA[i]
  R[b] <- cor(LSAT, GPA)
}
print(se.R <- sd(R))
hist(R, prob = TRUE)

library(boot) #for boot function
r <- function(x, i) {
  cor(x[i,1], x[i,2])
}
obj <- boot(data = law, statistic = r, R = 2000)
obj
