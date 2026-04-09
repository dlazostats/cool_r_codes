library(boot)
library(e1071)
library(bootstrap)
skew_fun<-function(data,indx){
  db<-data[indx,]
  sk<-skewness(db$Fluencia)
}
boot_s<-boot(train,skew_fun,R=1000)
skewness(train$Fluencia)
boot_s
sd(boot_s$t)
mean(boot_s$t)-skewness(train$Fluencia)
boot.ci(boot_s)
jk <- jackknife(1:nrow(train), skew_fun, data = train)
jk$jack.se
jk$jack.bias
hist(jk$jack.values,breaks="FD")
hist(boot_s$t,breaks="FD")
