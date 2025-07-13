## All of statistics
#--------------------
library(dplyr)
library(ACSWR)

# Cumulative distributions
data(nerve)
ecdf<- ecdf(nerve)
plot(ecdf)
ecdf(0.5) #evaluates

## by hand
sort_d<-sort(nerve)
n<-length(sort_d)
ecdf_bh<-seq(1,n)/n
df_ecdf<-data.frame(x=sort_d,F_x=ecdf_bh)
plot(df_ecdf$x,df_ecdf$F_x,type="s")
ecdf_byh<-function(d){
  linear_interpolate <- function(x0, x1, y1, x2, y2) {
    y0 <- y1 + ((x0 - x1) * (y2 - y1)) / (x2 - x1)
    return(y0)
  }
  sort_d<-sort(nerve)
  n<-length(sort_d)
  ecdf_bh<-seq(1,n)/n
  df_ecdf<-data.frame(x=sort_d,F_x=ecdf_bh)
  r<-df_ecdf %>% filter(x==d)
  if(nrow(r)==0){
    x1<-d-0.01
    x2<-d+0.01
    y1<-df_ecdf %>% filter(x==x1)
    y2<-df_ecdf %>% filter(x==x2)
    r<-linear_interpolate(d,x1,y1,x2,y2)
  }
  return(max(r$F_x))
}

# compare
ecdf(0.5)
ecdf_byh(0.5)

ecdf(0.2)
ecdf_byh(0.2)

ecdf(0.4)
ecdf_byh(0.4)

ecdf(0.6)-ecdf(0.4)
ecdf_byh(0.6)-ecdf_byh(0.4)

# Intervalo de confianza
alpha=0.05
e<-sqrt((1/(2*n))*log(2/alpha))
#dt_cfd<-df_ecdf %>% mutate(L_Inf=max(F_x-e,0),L_sup=min(F_x+e,1))
dt_cfd<-df_ecdf %>% mutate(L_Inf=F_x-e,L_sup=F_x+e) %>% mutate(L=max(L_Inf,0),U=min(L_sup,1))
plot(dt_cfd$x,dt_cfd$F_x,type="l")
lines(dt_cfd$L_sup,col="red")
lines(dt_cfd$L_Inf,col="red")
