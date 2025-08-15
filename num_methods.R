# Numerical Methods
#-------------------
library(rootSolve)
library(pracma)

# a) Bisection method
#=====================
bisec_mthd<-function(a,b,tol,func){
  results <- data.frame(iteration = integer(),a = numeric(),
                        b = numeric(),m = numeric(),f_m = numeric())
  iter<-1
  while(abs(b-a)>=tol){
    m=(a+b)/2
    f_m<-func(m)
    results <- rbind(results,data.frame(iteration = iter,a = a,b = b,m = m,f_m = f_m))
    if(func(m)==0){
      break
    }else if (func(b)*f_m<0){
      a<-m
    }else{
      b<-m
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

bisec_mthd(1.5,2,0.001,func)


## Example 2
func = function(x){
  x^3 - 10*x + 4
}
curve(func, xlim=c(-4,4), col = 'red', lwd = 2, lty = 2, xlab = "x", ylab = "f(x)")
abline(h=0)
abline(v=0)

bisec_mthd(-4,-3,0.0001,func)
bisec_mthd(0,1,0.0001,func)
bisec_mthd(2.1,3,0.0001,func)
uniroot.all(func, c(-5, 5))
bisect(func, 0, 1, tol = 1e-8)

# b) Secant Method
#=====================
secant_mthd<-function(x0,x1,tol,func,Nmax=100){
  for( i in 1:Nmax){
    f_x0<-func(x0)
    f_x1<-func(x1)
    if(abs(f_x1-f_x0)<.Machine$double.eps){
      stop("Denominator too small; division by near-zero.")
    }
    x2<-x1-f_x1*(x1-x0)/(f_x1 - f_x0)
    if(abs(x2-x1)<tol){
      return(list(root=x2,iterations=i,converged=TRUE))
    }
    x0<-x1
    x1<-x2
  }
  return(list(root=x2,iterations=i,converged=FALSE))
}  

func = function(x){ (x/2)^2-sin(x) }
secant_mthd(1.5,2,0.0001,func)
  







