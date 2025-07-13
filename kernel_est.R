# Kernel Density estimation
#----------------------------
set.seed(14012021)
data<-rnorm(200,mean=4)

#kernel 
dk<-density(data)
plot(dk,lwd=2,main="default kernel density")

# Different types of kernel estimation
types<-c("rectangular", "triangular", "epanechnikov", "biweight", "cosine",  "optcosine")
kern_des<-function(type="gaussian",bd="nrd"){
  dkk<-density(data,kernel=type,bw=bd)
  plot(dkk,lwd=2,main=paste0(type," ","Kernel", " with bw ", bd))
}
par(mfrow=c(2,3))
lapply(types,kern_des)

# bandwith selection
bdt<-c("nrd","ucv","bcv","SJ")
par(mfrow=c(2,2))
lapply(bdt,kern_des,type="gaussian")
