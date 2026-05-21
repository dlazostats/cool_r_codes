# Linear Algebra
#----------------
library(cmna)
library(matrixcalc)

# matrix multiplication
A <- matrix(1:9, 3)
B <- matrix(1:6, 3)
A %*% B

diag(A)
diag(2,4)
diag(1,3)

# matrix decomposition
A <- matrix(c(0, 1, 7, 1, 5, -1, -2, 9, -5), 3)
A
LUdecomp<-lumatrix(A)
LUdecomp$L
LUdecomp$U
LUdecomp$P %*% LUdecomp$L %*% LUdecomp$U
solve(LUdecomp$P) %*% LUdecomp$L %*% LUdecomp$U
A

# QR decomposition
QRdecomp<-qr(A)
Q <- qr.Q(QRdecomp)
R <- qr.R(QRdecomp)
set.seed(42)
n <- 50
x <- seq(1, 10, length.out=n)
b <- 2*x + 3 + rnorm(n)
dtf<-data.frame(y=b,x=x)
coef(lm(y~x,data=dtf))

# Design matrix [intercept | x]
A <- cbind(1, x)
qr_decomp <- qr(A) # QR least squares
x_hat <- qr.solve(A, b)
cat("Intercept:", x_hat[1], "\n")  # ≈ 3
cat("Slope:    ", x_hat[2], "\n")  # ≈ 2

# cholesky
A <- matrix(c(5, 1, 2, 1, 9, 3, 2, 3, 7), 3)
L <- choleskymatrix(A)
L
t(L) %*% L
A

# norm of a vector
vecnorm <- function (b) {
  return ( sqrt ( sum ( b ^2) ) )
}
x <- c(4, 8, 7, 2)
vecnorm(x) # euclidean norm

# Least Squares
data("trees")
head(trees)

A <- cbind(1, trees$Girth, trees$Height) 
b<-trees$Volume
solvematrix(t(A) %*% A, t(A) %*% b)
coef(lm(Volume~Girth+Height,data=trees))

# qrde<-qr(A)
Q<-qr.Q(qrde)
R<-qr.R(qrde)
Qtb <- t(Q)%*%b 
x_hat <- backsolve(R, Qtb)
x_hat
qr.solve(A,b)











