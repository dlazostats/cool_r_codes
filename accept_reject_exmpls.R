# acceptance - rejection method
#-------------------------------
# estimate from beta distribution
examp_opt<-function(){
  ft<-function(x)-x^2+2
  plot(seq(-2,2,0.1),ft(seq(-2,2,0.1)),type="l")
  optimize(f=function(x){-x^2+2},interval=c(-2,2),maximum=T)
}
examp_opt()
M<-optimize(f=function(x){dbeta(x,2.7,6.3)},interval=c(0,1),maximum=T)$objective
M
