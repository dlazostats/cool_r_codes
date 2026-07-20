library(dplyr)
library(caret)
library(psych)
library(tidymodels)
library(MLmetrics)
library(kernlab)

# setworking directory
setwd("D:/OneDrive - CORPORACIÓN ACEROS AREQUIPA SA/Escritorio/pract ml")

# load data
db0<-read.csv("train_qtb.csv")
db1<-db0 %>% select(RES_TRA ,PE_MET,Temp_lam,C:material_c)
head(db1);dim(db1)

#EDA
set.seed(2121)
samp1<-db1 %>% 
       filter(material_c==0.233242244012217 & PE_MET==0.945) %>%
       filter(Temp_lam>680 & Temp_lam<700) #%>% 
       #dplyr::slice_sample(n=900)
{
  regVar <- sample(names(samp1 %>% select(-RES_TRA)),3)
  featurePlot(x = samp1[, regVar], 
              y = samp1$RES_TRA, 
              plot = "scatter",
              type = c("p", "smooth"),
              span = .5,
              layout = c(3, 1))  
}
ggplot(samp1,aes(x=Cr,y=Mo))+
  geom_point()+
  geom_smooth(se=F)+
  theme_bw()

# split into train/test
set.seed(2121)
splitt<-initial_split(samp1,prop=0.8,strata=Mo)
train<-training(splitt)
test<-testing(splitt)
combined <- bind_rows(
  train %>% mutate(set = "train"),
  test  %>% mutate(set = "test")
)
ggplot(combined, aes(x = RES_TRA, fill = set, color = set)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  theme_minimal() +
  labs(title = "Target density: train vs test", x = "target")

#basic example
d <- data.frame(x = train$Cr, y = train$Mo)
fit <- gausspr(y ~ x, data = d, kernel = "rbfdot",
               kpar = "automatic")
mu <- predict(fit, data.frame(x = test$Cr), type = "response")     
sd <- predict(fit, data.frame(x = test$Cr), type = "sdeviation")  

plot(test$Cr, test$Mo, pch = 19, col = "grey40",
     xlab = "Cr", ylab = "Mo", main = "GP regression fit")
o <- order(test$Cr)                       
lines(test$Cr[o], mu[o], col = "blue", lwd = 2)

library(GauPro)
gp <- GauPro(matrix(train$Cr, ncol = 1), train$Mo)
pr <- gp$predict(matrix(test$Cr, ncol = 1), se.fit = TRUE)
mu <- pr$mean; sd <- pr$se
plot(gp)


library(GauPro)
gp <- GauPro(matrix(train$Cr, ncol = 1), train$Mo)

pr <- gp$predict(matrix(test$Cr, ncol = 1), se.fit = TRUE)
mu <- pr$mean
sd <- pr$se

o <- order(test$Cr)
x <- test$Cr[o]; m <- mu[o]; s <- sd[o]

plot(test$Cr, test$Mo, pch = 19, col = "grey40",
     xlab = "Cr", ylab = "Mo", main = "GauPro GP fit",
     ylim = range(c(test$Mo, m + 2*s, m - 2*s)))
polygon(c(x, rev(x)), c(m + 2*s, rev(m - 2*s)),
        col = rgb(0, 0, 1, 0.15), border = NA)   # 95% band
lines(x, m, col = "blue", lwd = 2)
points(test$Cr, test$Mo, pch = 19, col = "grey40")  # redraw on top



