## Quantile Regression
#-----------------------
library(quantregForest)
library(dplyr)
library(MLmetrics)
library(summarytools)
library(caret)
library(sjstats)

calculate_mad <- function(x) {
  median(abs(x - median(x)))
}
calculate_scaled_mad <- function(x) {
  1.4826 * median(abs(x - median(x)))
}
# set working directory
script_name <- 'RFquantreg2.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0[sample(1:nrow(db0),1500),] %>% dplyr::select(Fluencia,PesoMetrico:Pb,material_c)

# quick EDA
hist(db0_sqmple$Fluencia,breaks = "FD")
descr(db0_sqmple)

# Quantile Random Forest
set.seed(212)
indx<-createDataPartition(db0_sqmple$Fluencia,p=0.8,list=F)
train<-db0_sqmple[indx,]
test<-db0_sqmple[-indx,]
Xtrain<-train %>% dplyr::select(-Fluencia)
Ytrain<-train %>% dplyr::select(Fluencia) %>% pull()
Xtest<-test %>% dplyr::select(-Fluencia)
Ytest<-test %>% dplyr::select(Fluencia)%>% pull()

qrf <- quantregForest(x=Xtrain, y=Ytrain,keep.inbag=TRUE )
qrf
plot(qrf)

conditionalSd <- predict(qrf,  Xtest, what=sd)
conditionalMean <- predict(qrf,  Xtest, what=mean)
conditionalquant  <- predict(qrf, newdata= Xtest) %>% as.data.frame()

plot(Ytest,type="l")
lines(conditionalquant$`quantile= 0.1`,col="red")
lines(conditionalquant$`quantile= 0.9`,col="red")

###coverage
conditionalquant %>% 
  mutate(real=Ytest) %>% 
  mutate(covid=ifelse(real>=`quantile= 0.1` & real<=`quantile= 0.9`,1,0)) %>% 
  summarise(mean_cov=mean(covid))

## interval width
dtinv<-conditionalquant %>% 
       mutate(int_width=`quantile= 0.9`-`quantile= 0.1`)

hist(dtinv$int_width,breaks="FD")
median(dtinv$int_width)
sd(dtinv$int_width)
gmd(dtinv$int_width)
calculate_mad(dtinv$int_width)
calculate_scaled_mad (dtinv$int_width)

skewed_data<-dtinv$int_width
measures <- data.frame(
  Method = c("Standard Deviation", 
             "Interquartile Range (IQR)", 
             "Median Absolute Deviation (MAD)",
             "Scaled MAD",
             "10-90 Percentile Range",
             "GINI"),
  Value = c(
    sd(skewed_data),
    IQR(skewed_data),
    calculate_mad(skewed_data),
    calculate_scaled_mad(skewed_data),
    diff(quantile(skewed_data, c(0.1, 0.9))),
    gmd(skewed_data)
  )
)
measures 


#Plot
plot(Ytest,type="p")
lines(Ytest,col="grey")
lines(conditionalMean,col="red")            
lines(conditionalquant$`quantile= 0.1`,col="blue")
lines(conditionalquant$`quantile= 0.9`,col="blue")

# Metrics
