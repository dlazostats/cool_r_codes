## Deepnet and mxnet
#--------------------
library(deepnet)
library(tidyverse)
library(tidymodels)
library(mlbench)
library(neuralnet)

## Deepnet
#----------
data("BreastCancer")
BCrec <- recipe(Class ~ ., data = BreastCancer) %>%
  step_naomit(all_predictors()) %>%
  prep(BreastCancer, verbose = FALSE) %>%
  bake(new_data = NULL)
BCrec

y <- BCrec %>% select(Class) %>% mutate(Class = ifelse(Class == "benign",0,1)) %>% as.matrix()
x <- BCrec %>% select(-Class,-Id) %>% mutate_all(as.numeric) %>% as.matrix()

nn <- nn.train(x, y, hidden = c(5))
yy = nn.predict(nn, x)
head(yy)

yhat <- yy %>% as_tibble() %>% mutate(yy = ifelse(yy > mean(yy),1,0)) %>% select(yy) %>% as.matrix()
cm <- table(y,yhat)
cm
print(sum(diag(cm))/sum(cm))

## Neuralnet
#------------
df <- data.frame(cbind(x,y)) 
neuraln <- neuralnet(y~.,data=df,hidden = 5)
yy2 <- neuraln$net.result[[1]]
yhat2 <- yy2 %>% as_tibble() %>% mutate(yy2 = ifelse(yy2 > mean(yy2),1,0)) %>% select(yy2) %>% as.matrix()
cm2 <- table(y,yhat2)
cm2
print(sum(diag(cm2))/sum(cm2))
