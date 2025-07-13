library(sjstats)

#ejemplo bb
var<-mtcars$mpg
gmd(var)
sd(var)
n<-length(var)
fct<-2/(n*(n-1))
df<-expand.grid(var,var)
df$id<-ifelse(df$Var1==df$Var2,0,1)
df_s<-df[df$id==1,]
df_s$abs1<-abs(df_s$Var2-df_s$Var1)
mean(df_s$abs1)
((1/choose(n,2)))*sum(df_s$abs1)

# ejemplo mtcars
dft<-mtcars[,c("cyl","mpg")]
dft$mpg_or<-dft$mpg
n<-nrow(dft)
fct<-2/(n*(n-1))

df<-expand.grid(mtcars$mpg,mtcars$mpg)
hist(mtcars$disp)
sd(mtcars$disp)
gmd(mtcars$disp)
mean(mtcars$disp)
median(mtcars$disp)

# Gini Index
#------------
library(sjstats)
library(Hmisc)

# Excersice 1 
var<-c(2,3,5,6,10,15,1,14,2,2,3,1,3)
hist(var)
sd(var)
gmd(var)
GiniMd(var)

gmd <- function(x) {
  n <- length(x)
  sum(outer(x, x, function(a, b) abs(a - b))) / n / (n - 1)
}
x<-var
n <- length(x)
sum(outer(x, x, function(a, b) abs(a - b)))/n/(n - 1)

x<-c(3,2,4)
sum(outer(x, x, function(a, b) abs(a - b)))/3/2
gmd(x)
GiniMd(x)
