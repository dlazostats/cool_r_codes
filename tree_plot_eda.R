library(rpart)
library(parttree)
library(partykit)
fittb0 = rpart(mpg  ~ hp + wt, data = mtcars)
fit_pt = parttree(fittb0)
ct = ctree(mpg  ~ hp + wt, data = mtcars)
plot(ct)
plot(fit_pt)

ct = ctree(Species ~ Petal.Length + Petal.Width, data = iris)
plot(ct)
ct_pt = parttree(ct)
plot(ct_pt, pch = 19, palette = "okabe", main = "ctree predictions: iris species")

