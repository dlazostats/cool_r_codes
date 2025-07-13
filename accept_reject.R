# Acceptance-rejection method
#-----------------------------
# example 1
## For the beta distribution
# beta(2,2) f(x)= 6x(1-x) [0,1], sea g(x) unif(0,1)
f<-function(x) 6*x*(1-x)
x<-seq(0,1,0.01)
plot(x,f(x),type="l")

n<-1000
k<-0 #counter for accepted
j<-0
y<-numeric()
while(k<n){
  u<-runif(1)
  j<-j+1
  x<-runif(1)
  if(x*(1-x)>u){
    k<-k+1 #we accept x
    y[k]<-x
  }
}
j

#compare
p<-seq(.1,.9,.1)
qhat<-quantile(y,p)
q<-qbeta(p,2,2)
se<-sqrt(p*(1-p)/n)
round(rbind(qhat,q,se),3)

#example 2
f<-function(x) exp(-x)
x<-seq(0,2,0.1)
plot(x,f(x),type="l")

n<-1000
k<-0 #counter for accepted
j<-0
y<-numeric()
while(k<n){
  u<-runif(1)
  j<-j+1
  x<-runif(1)
  if(exp(-x)>u){
    k<-k+1 #we accept x
    y[k]<-x
  }
}

p<-seq(.1,.9,.1)
qhat<-quantile(y,p)
q<-qexp(p,2,2)
se<-sqrt(p*(1-p)/n)
round(rbind(qhat,q,se),3)


## more formally (chatgpt)
target_density <- function(x) {
  exp(-x)  # f(x) = e^{-x}
}
# Define the proposal distribution: Uniform on [0, b]
b <- 2
proposal_density <- function(x) {
  1 / b  # g(x) = 1/b for x in [0, b]
}

# Acceptance probability calculation
acceptance_probability <- function(x) {
  target_density(x) / (b * proposal_density(x))  # f(x) / (c * g(x)) = e^{-x} / (b * (1/b))
}

# Generate a sample using the acceptance-rejection method
generate_sample <- function() {
  while (TRUE) {
    X <- runif(1, 0, b)  # Draw from Uniform(0, b)
    U <- runif(1)        # Draw from Uniform(0, 1)
    
    if (U <= acceptance_probability(X)) {
      return(X)  # Accept X as a sample from the exponential distribution
    }
    # Otherwise, reject X and repeat
  }
}

# Generate multiple samples
set.seed(42)  # For reproducibility
n_samples <- 1000
samples <- replicate(n_samples, generate_sample())

# Plotting the histogram of the samples against the true exponential density
heist(samples, breaks = 30, probability = TRUE, col = "lightblue", main = "Histogram of Accepted Samples", xlab = "Sample values")
curve(exp(-x), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Exponential Density", "Samples"), col = c("red", "lightblue"), lwd = c(2, NA), fill = c(NA, "lightblue"))



