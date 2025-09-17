library(purrr)
set.seed(12)

# Function to generate data
generate_data <- function(beta, sig, n) {
  u <- runif(n, 0, 1)   # uniform(0,1)
  # y = sum_{j=0}^3 u^j * beta_j  + noise
  X <- outer(u, 0:(length(beta)-1), `^`)   # Vandermonde-like matrix
  y <- X %*% beta + sig * rnorm(n)
  return(list(u = u, y = y))
}

# Parameters
beta <- matrix(c(10, -140, 400, -250), ncol = 1)
n <- 100
sig <- 5

# Generate data
dat <- generate_data(beta, sig, n)
u <- dat$u
y <- dat$y

# Generate smooth curve (like np.polyval)
xx <- seq(min(u), max(u) + 5e-3, by = 5e-3)
yy <- polyval <- function(beta, x) {
  # reverse beta for R's polynomial evaluation
  sapply(x, function(xi) sum(beta * xi^(0:(length(beta)-1))))
}
yy <- polyval(beta, xx)

# Plot
plot(u, y, pch = 19, cex = 0.8, xlab = "u", ylab = expression(h^"*"(u)))
lines(xx, yy, lty = 2, lwd = 3, col = "blue")
legend("topright", legend = c("data", "true curve"), 
       col = c("black", "blue"), pch = c(19, NA), lty = c(NA, 2), lwd = c(NA, 3))

# train-test to select best polynomial regression
df_mdl<-data.frame(y=y,u=u)

set.seed(1212)
indx<-sample(1:nrow(df_mdl),ceiling(dim(df_mdl)[1]*0.8))
train<-df_mdl[indx,]
test<-df_mdl[-indx,]
cv=4
folds<-sample(rep(1:cv, length.out = nrow(train)))

fun_cv_poly<-function(deg){
  degree<-deg
  mse_list <- numeric(cv)
  for(k in 1:cv){
    testindx<-which(folds==k)
    train_cv<-train[-testindx,]
    test_cv<-train[testindx,]
    
    mdl_poly<-lm(y~poly(u,degree,raw=T),data=train_cv)
    pred_poly<-predict(mdl_poly,test_cv)
    mse<-mean((test_cv$y-pred_poly)^2)
    mse_list[k]<-mse
  }
  prom_rmseq<-mean(mse_list)
  res_cv<-c(degree,prom_rmseq)
  return(res_cv)
}
df_res_poly<-map(1:14,fun_cv_poly) %>% do.call("rbind",.) %>% as.data.frame()
plot(df_res_poly$V2,type="l")

yy_p<-predict(mdl_poly,newdata=data.frame(u=u))
ord <- order(u)
plot(u, y, pch = 19, cex = 0.8, xlab = "u", ylab = expression(h^"*"(u)))
lines(u[ord], yy_p[ord], lty = 2, lwd = 3, col = "blue")

