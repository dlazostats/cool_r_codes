dt<-data.frame(optden=c(0.086,0.269,0.449,0.538,0.626,0.782),
               carb=c(0.1,0.3,0.5,0.6,0.7,0.9))
X<-model.matrix(lm1<-lm(optden~carb,data=dt))

lm1$qr
R<-qr.R(lm1$qr)
Q1<-qr.Q(lm1$qr)
coef(lm1)
solve(R)%*%t(Q1)%*%dt$optden

#full Q matrix
Q<-qr.Q(lm1$qr,complete=T)

#checka that Q is orthognal QTQ=I
crossprod(Q1)
crossprod(Q)
