library(disttree)
library(rpart)
library(ranger)
library(dplyr)

n <- 500
set.seed(12345) 
{
  x <- runif(n=n)
  y <- numeric(n)
  y[x < 0.4]            <- rnorm(n=sum(x < 0.4),            mean=5, sd=1)
  y[x >+ 0.4 & x < 0.8] <- rnorm(n=sum(x >+ 0.4 & x < 0.8), mean=12, sd=2)
  y[x >= 0.8]           <- rnorm(n=sum(x >= 0.8),           mean=0, sd=0.5)
}

datos <- data.frame(y=y, x=x)
plot(x=x, y=y, ylim=c(-5, 20))

mod <- disttree(y ~ x, data=datos, family=NO)
plot(mod)

new_data <- data.frame(x=c(0.35, 0.47, 0.89))
predicciones <- predict(mod, newdata=new_data)
predicciones

n <- 100
k <- 2
set.seed(12345) 
{
  x <- runif(n=n, min=-0.4, max=1)
  media <- 10 * exp(-(4*x-2)^(2*k))
  desvi <- 0.5 + 2 * abs(x)
  y <- rnorm(n=n, mean=media, sd=desvi)
}

datos <- data.frame(y, x)
head(datos)
plot(x=x, y=y, pch=19)
mod1 <- disttree(y ~ x, data=datos, family=NO)
mod2 <- distforest(y ~ x, data=datos, family=NO)
newdata <- data.frame(x=seq(from=-0.4, to=1, length.out=100))


y_hat_1 <- predict(mod1, newdata=newdata)$mu # El resultado es mu y sigma
y_hat_2 <- predict(mod2, newdata=newdata)$mu # elegimos solo mu

plot(x=x, y=y, pch=19)
lines(x=newdata$x, y=y_hat_1, col="tomato", lwd=3)
lines(x=newdata$x, y=y_hat_2, col="chartreuse3", lwd=3)
legend("topleft", legend=c("Arbol", "Bosque"),
       lwd=3, col=c("tomato", "chartreuse3"))

## Mean values
# regression trees
tr1<-rpart(y~x,data=datos)
pred_tr1<-predict(tr1,newdata=newdata)

plot(x=x, y=y, pch=19)
lines(x=newdata$x, y=pred_tr1, col="tomato", lwd=3)
lines(x=newdata$x, y=y_hat_1, col="chartreuse3", lwd=3)
legend("topleft", legend=c("rpart", "disttree"),
       lwd=3, col=c("tomato", "chartreuse3"))

# Random Forest
rf1<-ranger(y~x,data=datos)
preed_rf1<-predict(rf1, data = newdata)$predictions

plot(x=x, y=y, pch=19)
lines(x=newdata$x, y=preed_rf1, col="tomato", lwd=3)
lines(x=newdata$x, y=y_hat_2, col="chartreuse3", lwd=3)
legend("topleft", legend=c("ranger", "distforest"),
       lwd=3, col=c("tomato", "chartreuse3"))

## Variance 
### distttree
y_hat_11 <- predict(mod1, newdata=newdata)$sigma # El resultado es mu y sigma
y_hat_22 <- predict(mod2, newdata=newdata)$sigma # elegimos solo mu

l11<-y_hat_1+y_hat_11
l12<-y_hat_1-y_hat_11
plot(x=x, y=y, pch=19)
lines(x=newdata$x, y=y_hat_1, col="tomato", lwd=3)
lines(x=newdata$x, y=l11, col="chartreuse3", lwd=3)
lines(x=newdata$x, y=l12, col="chartreuse3", lwd=3)

## Distforest
l21<-y_hat_2+y_hat_22
l22<-y_hat_2-y_hat_22
plot(x=x, y=y, pch=19)
lines(x=newdata$x, y=y_hat_2, col="tomato", lwd=3)
lines(x=newdata$x, y=l21, col="chartreuse3", lwd=3)
lines(x=newdata$x, y=l22, col="chartreuse3", lwd=3)

library(party)

# set working directory
script_name <- 'disttree.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

db0<-read.csv("dftrain.csv")
db1<-db0 %>% select(Fluencia,PesoMetrico:Pb,material_c)

#MOB
ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 40,
                    verbose = TRUE)
fmBH <- mob(Fluencia ~ C|PesoMetrico+material_c, data = db1, 
            control = ctrl, model = glm,family = gaussian())


library(partykit)

fmBH <- partykit::mob(
  Fluencia ~ C | PesoMetrico + TempLam,
  data = db1,
  fit = glm,
  control = ctrl
)


data("BostonHousing", package = "mlbench")
## and transform variables appropriately (for a linear regression)
BostonHousing$lstat <- log(BostonHousing$lstat)
BostonHousing$rm <- BostonHousing$rm^2
## as well as partitioning variables (for fluctuation testing)
BostonHousing$chas <- factor(BostonHousing$chas, levels = 0:1, 
                             labels = c("no", "yes"))
BostonHousing$rad <- factor(BostonHousing$rad, ordered = TRUE)

## partition the linear regression model medv ~ lstat + rm
## with respect to all remaining variables:
fmBH <- mob(medv ~ lstat + rm | zn + indus + chas + nox + age + 
              dis + rad + tax + crim + b + ptratio,
            control = mob_control(minsplit = 40), data = BostonHousing, 
            model = linearModel)





