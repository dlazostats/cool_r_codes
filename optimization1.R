# Optimization
#----------------
library(nloptr)

# objective function
eval_f <- function(x){
  return ( 100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2 )
}

# gradient
eval_grad_f <- function(x) {
  return( c( -400 * x[1] * (x[2] - x[1] * x[1]) - 2 * (1 - x[1]),
             200 * (x[2] - x[1] * x[1]) ) )
}

# initial values
x0 <- c( -1.2, 1 )

# optimization procedure
opts <- list("algorithm"="NLOPT_LD_LBFGS",
             "xtol_rel"=1.0e-8)
opt_res <- nloptr( x0=x0,
                   eval_f=eval_f,
                   eval_grad_f=eval_grad_f,
                   opts=opts)
opt_res

# Minimization with inequality constrain and without gradients
# objective function
eval_f0 <- function( x, a, b ){
  return( sqrt(x[2]) )
}
# constraint function
eval_g0 <- function( x, a, b ) {
  return( (a*x[1] + b)^3 - x[2] )
}
# define parameters
a <- c(2,-1)
b <- c(0, 1)

# Solve using NLOPT_LN_COBYLA
res1 <- nloptr( x0=c(1.234,5.678),
                eval_f=eval_f0,
                lb = c(-Inf,0),
                ub = c(Inf,Inf),
                eval_g_ineq = eval_g0,
                opts = list("algorithm"="NLOPT_LN_COBYLA",
                            "xtol_rel"=1.0e-8),
                a = a,
                b = b )
print( res1 )

# Minimization with multiple inequality constraints without gradients
eval_f <- function(x)
{
  return ( x[1]^2 + x[2]^2 )
}
# Inequality constraints
eval_g_ineq <- function (x) {
  constr <- c(1 - x[1] - x[2],
              1 - x[1]^2 - x[2]^2,
              9 - 9*x[1]^2 - x[2]^2,
              x[2] - x[1]^2,
              x[1] - x[2]^2)
  return (constr)
}
lb <- c(-50, -50)
ub <- c(50, 50)
x0 <- c(3, 1)
opts <- list( "algorithm" = "NLOPT_GN_ISRES",
              "xtol_rel" = 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 5 ))
res <- nloptr(
  x0 = x0,
  eval_f = eval_f,
  lb = lb,
  ub = ub,
  eval_g_ineq = eval_g_ineq,
  opts = opts )
print(res)














