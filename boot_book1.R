# Introduccion to the bootstrap
#------------------------------
library(boot)

patch <- data.frame(
  subject = 1:8,
  placebo = c(9243,9671,11792,13357,9055,6290,12412,18806),
  oldpatch = c(17649,12013,19979,21816,13850,9806,17208,29044),
  newpatch = c(16449,14614,17274,23798,12560,10157,16570,26325)
)

patch$z <- patch$oldpatch - patch$placebo
patch$y <- patch$newpatch - patch$oldpatch
theta_h<-mean(patch$y)/mean(patch$z)

set.seed(2101)
funbot<-function(data,index){
  bd<-data[index,]
  return(mean(bd$y)/mean(bd$z))
}
botres1<-boot(patch,funbot,R=400)
hist(botres1$t,breaks="FD")

botres1
theta_h
mean(botres1$t)-theta_h #bias
sd(botres1$t)

# ratio bias/se  (if the ratio <0.025 standard erros can be ifnored)
c(mean(botres1$t)-theta_h )/sd(botres1$t)
boot.ci(botres1)
