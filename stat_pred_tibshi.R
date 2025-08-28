# Statistical Prediction
#========================
# Sample - splitting
#---------------------
dat = read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/xy.dat")
head(dat)
n<-nrow(dat)
set.seed(0)
inds<-sample(rep(1:2,length=n))

dat.tr = dat[inds==1,] # Training data
dat.te = dat[inds==2,] # Test data

plot(dat$x, dat$y, pch=c(21,19)[inds], main="Sample-splitting")
legend("topleft", legend=c("Training","Test"), pch=c(21,19))

# Train on the first half
lm.1 = lm(y ~ x, data=dat.tr)
lm.10 = lm(y ~ poly(x,10), data=dat.tr)

# Predict on the second half, evaluate test error
pred.1 = predict(lm.1, data.frame(x=dat.te$x))
pred.10 = predict(lm.10, data.frame(x=dat.te$x))

test.err.1 = mean((dat.te$y - pred.1)^2)
test.err.10 = mean((dat.te$y - pred.10)^2)

# Plot the results
par(mfrow=c(1,2))
xx = seq(min(dat$x), max(dat$x), length=100)

plot(dat$x, dat$y, pch=c(21,19)[inds], main="Sample-splitting")
lines(xx, predict(lm.1, data.frame(x=xx)), col=2, lwd=2)
legend("topleft", legend=c("Training","Test"), pch=c(21,19))
text(0, -6, label=paste("Test error:", round(test.err.1,3)))

plot(dat$x, dat$y, pch=c(21,19)[inds], main="Sample-splitting")
lines(xx, predict(lm.10, data.frame(x=xx)), col=3, lwd=2)
legend("topleft", legend=c("Training","Test"), pch=c(21,19))
text(0, -6, label=paste("Test error:", round(test.err.10,3)))

# Split data in 5 parts, randomly
k = 5
set.seed(0)
inds = sample(rep(1:k, length=n))
head(inds, 10)

# Now run cross-validation: easiest with for loop, running over
# which part to leave out
pred.mat = matrix(0, n, 2) # Empty matrix to store predictions
for (i in 1:k) {
  cat(paste("Fold",i,"... "))
  
  dat.tr = dat[inds!=i,] # Training data
  dat.te = dat[inds==i,] # Test data
  
  # Train our models
  lm.1.minus.i = lm(y ~ x, data=dat.tr)
  lm.10.minus.i = lm(y ~ poly(x,10), data=dat.tr)
  
  # Record predictions
  pred.mat[inds==i,1] = predict(lm.1.minus.i, data.frame(x=dat.te$x))
  pred.mat[inds==i,2] = predict(lm.10.minus.i, data.frame(x=dat.te$x))
}
pred.mat
cv.errs = colMeans((pred.mat - dat$y)^2)

# Plot the results
par(mfrow=c(1,2))
xx = seq(min(dat$x), max(dat$x), length=100)

plot(dat$x, dat$y, pch=20, col=inds+1, main="Cross-validation")
lines(xx, predict(lm.1, data.frame(x=xx)), # Note: model trained on FULL data!
      lwd=2, lty=2) 
legend("topleft", legend=paste("Fold",1:k), pch=20, col=2:(k+1))
text(0, -6, label=paste("CV error:", round(cv.errs[1],3)))

plot(dat$x, dat$y, pch=20, col=inds+1, main="Cross-validation")
lines(xx, predict(lm.10, data.frame(x=xx)), # Note: model trained on FULL data!
      lwd=2, lty=2) 
legend("topleft", legend=paste("Fold",1:k), pch=20, col=2:(k+1))
text(0, -6, label=paste("CV error:", round(cv.errs[2],3)))

# Now we visualize the different models trained, one for each CV fold
for (i in 1:k) {
  dat.tr = dat[inds!=i,] # Training data
  dat.te = dat[inds==i,] # Test data
  
  # Train our models
  lm.1.minus.i = lm(y ~ x, data=dat.tr)
  lm.10.minus.i = lm(y ~ poly(x,10), data=dat.tr)
  
  # Plot fitted models
  par(mfrow=c(1,2)); cols = c("red","gray")
  plot(dat$x, dat$y, pch=20, col=cols[(inds!=i)+1], main=paste("Fold",i))
  lines(xx, predict(lm.1.minus.i, data.frame(x=xx)), lwd=2, lty=2) 
  legend("topleft", legend=c(paste("Fold",i),"Other folds"), pch=20, col=cols)
  text(0, -6, label=paste("Fold",i,"error:",
                          round(mean((dat.te$y - pred.mat[inds==i,1])^2),3)))
  
  plot(dat$x, dat$y, pch=20, col=cols[(inds!=i)+1], main=paste("Fold",i))
  lines(xx, predict(lm.10.minus.i, data.frame(x=xx)), lwd=2, lty=2) 
  legend("topleft", legend=c(paste("Fold",i),"Other folds"), pch=20, col=cols)
  text(0, -6, label=paste("Fold",i,"error:",
                          round(mean((dat.te$y - pred.mat[inds==i,2])^2),3)))
}

# statistical prediction
#-------------------------

