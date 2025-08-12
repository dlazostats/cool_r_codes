# Numerical Methods
#-------------------
# a) Bisection method
bisec_mthd<-function(a,b,tol,func){
  results <- data.frame(iteration = integer(),a = numeric(),
                        b = numeric(),m = numeric(),f_m = numeric())
  it<-1
  while(abs(b-a)>=tol){
    m=(a+b)/2
    f_m<-func(m)
    results <- rbind(results,data.frame(iteration = iter,a = a,b = b,m = m,f_m = f_m))
    if(func(m)==0){
      break
    }else if (func(b)*func(m)<0){
      a=m
    }else{
      b=m
    }
    iter <- iter + 1
  }
  return(results)
}


## Example 1
func = function(x){
  (x/2)^2-sin(x)
}
curve(func, xlim=c(1,3), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")
abline(h=0)
abline(v=0)


## Example 2
func = function(x){
  x^3 - 10*x + 4
}
curve(func, xlim=c(-4,4), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")
abline(h=0)
abline(v=0)

bisec_mthd <- function(a, b, tol, func) {
  results <- data.frame(iteration = integer(), a = numeric(), b = numeric(), m = numeric(), f_m = numeric())
  iter <- 1
  while (abs(b - a) >= tol) {
    m <- (a + b) / 2
    f_m <- func(m)
    results <- rbind(results, data.frame(iteration = iter, a = a, b = b, m = m, f_m = f_m))
    if (abs(f_m) < 1e-12) break
    if (func(b) * f_m < 0) {
      a <- m
    } else {
      b <- m
    }
    iter <- iter + 1
    if (iter > 10000) stop("Too many iterations; possible non-convergence.")
  }
  results
}
