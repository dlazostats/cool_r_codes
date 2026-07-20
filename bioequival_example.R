library(boot)
library(dplyr)
drug <- data.frame(
  patient  = 1:8,
  placebo  = c( 9243,  9671, 11792, 13357,  9055,  6290, 12412, 18806),
  approved = c(17649, 12013, 19979, 21816, 13850,  9806, 17208, 29044),
  new      = c(16449, 14614, 17274, 23798, 12560, 10157, 16570, 26325)
)
drug$app_pla <- drug$approved - drug$placebo
drug$new_app <- drug$new      - drug$approved
drug
sapply(drug %>% select(-patient),function(x) mean(x))

x<-drug$app_pla
y<-drug$new_app
m_x<-mean(x)
m_y<-mean(y)
ratio<-m_y/m_x

plot(x,y)
abline(h = 0, lty = 3) 
points(mean(x), mean(y), pch = 19, col = "red")

fun_ratio<-function(data,indx){
  db<-data[indx,]
  ratio<-mean(db$new_app)/mean(db$app_pla)
  return(ratio)
}
set.seed(2121)
resboot<-boot(drug,fun_ratio,R=4000)
hist(resboot$t,breaks="FD")
abline(v = ratio, lty = 3,col="red") 
resboot
mean(resboot$t);sd(resboot$t)
boot.ci(resboot, type = c("perc", "bca"))
boot.ci(resboot, type = c("bca"),conf=0.9)

r<-numeric()
for(i in 1:4000){
  indx<-sample(drug$patient,size=length(drug$patient),replace=T)
  db<-drug[indx,]
  ratio<-mean(db$new_app)/mean(db$app_pla)
  r[i]<-ratio
}
hist(r,breaks="FD")

# parametric
fun_ratio <- function(data) {
   mean(data$new_app)/mean(data$app_pla) 
}
ran_gen_norm <- function(data, mle) {
  out <- data
  out$app_pla <- rnorm(nrow(data), mean = mle$mu_app, sd = mle$sd_app)
  out$new_app <- rnorm(nrow(data), mean = mle$mu_new, sd = mle$sd_new)
  out
}
mle_params <- list(
  mu_app = mean(drug$app_pla), sd_app = sd(drug$app_pla),
  mu_new = mean(drug$new_app), sd_new = sd(drug$new_app)
)
set.seed(2121)
resboot_param <- boot(drug, fun_ratio, R = 4000,
                sim = "parametric",
                ran.gen = ran_gen_norm,
                mle = mle_params)
hist(resboot_param$t, breaks = "FD")
abline(v = ratio, lty = 3,col="red") 
resboot_param
boot.ci(resboot_param)
boot.ci(resboot_param, conf = 0.95, type = c("norm", "basic", "perc"))
mean(resboot_param$t);sd(resboot_param$t)
boot.ci(resboot_param, conf = 0.9, type = c("norm", "basic"))

# power 










