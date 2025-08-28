## boostrap
#------------
library(boot)
library(dplyr)
library(bootstrap) # from book

# Set working directory
script_name <- 'boot1_prop.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# basic example for mediam 
df0<-read.csv("c:/Users/dlazo/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/Diego/bootstrap/df_propmca.csv")
df1<-df0 %>% filter(RES_TRA<800 & RES_TRA>350)
hist(df1$RES_TRA)

# bootstrap for median by hand
boot_med<-function(var,q=1000){
  fun_samp<-function(var){
    n<-sample(var,length(var),replace = T)
    med<-median(n,na.rm = T)
  }
  med<-sapply(var,fun_samp)
  return(med)
}
med_b1<-boot_med(df1$RES_TRA,q=1000)
hist(med_b1)

# using the function
samplemedian <- function(x, d) {
  return(median(x[d]))
}
b_med <- boot(data = df1$RES_TRA,statistic = samplemedian,R = 1000)
hist(b_med$t,breaks="FD")
b_med
median(df0$RES_TRA)
boot.ci(b_med, type = "bca")

samplemean <- function(x, d) {
  return(median(x[d]))
}
b_med <- boot(data = df1$RES_TRA,statistic = samplemean,R = 1000)
hist(b_med$t,breaks="FD")

## jackinfe
# example1
x <- df1$RES_TRA               
theta <- function(x){mean(x)}
results <- jackknife(x,theta)    
mean(x)
sd(x)
sd(x)/sqrt(length(x))
fun_jack(x)
sd(bootstrap(x,1000,theta)$thetastar)

#example2
x<-mtcars$mpg
jackknife(x,theta)
fun_jack(x)
sd(bootstrap(x,1000,theta)$thetastar)
sd(x)/sqrt(length(x))


# other example
#----------------
library(boot)
library(ggplot2)
boot_result<-boot(data=mtcars$mpg,statistic=samplemedian,R=1000)
  
# Create data frame
boot_medians <- data.frame(median = boot_result$t)

# Get 95% BCa confidence interval
ci <- boot.ci(boot_result, type = "bca")

# Extract lower and upper bounds
lower_ci <- ci$bca[4]
upper_ci <- ci$bca[5]

# Plot
hist(boot_medians$median)
ggplot(boot_medians, aes(x = median)) +
  geom_histogram(
    bins = 30,
    color = "black",
    fill = "lightblue"
  ) +
  geom_vline(
    xintercept = median(mtcars$mpg),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    xintercept = c(lower_ci, upper_ci),
    color = "darkgreen",
    linetype = "dotted",
    size = 1
  ) +
  labs(
    title = "Bootstrap Distribution of the Median with 95% CI",
    x = "Bootstrapped Median",
    y = "Frequency"
  ) +
  theme_minimal()
