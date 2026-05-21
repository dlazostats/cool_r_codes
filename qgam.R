library(qgam)
library(MASS)
library(mgcViz)

head(mcycle)

fitCycle1 <- qgam(list(form = accel ~ s(times, k = 20, bs = "ad"),
                  ~ s(times)), data = mcycle, qu = 0.9)
plot(fitCycle1)

fitCycleM <- mqgam(list(form = accel ~ s(times, k = 20, bs = "ad"),
                    ~ s(times)), data = mcycle, qu = c(0.1, 0.25, 0.5, 0.75, 0.9))
qdo(fitCycleM, qu = 0.25, fun = summary)
fitCycleMa <- getViz(fitCycleM)
plot(fitCycleMa)
xseq <- with(mcycle, seq(min(times), max(times), length.out = 100))
preds <- sapply(fitCycleMa, predict, newdata = data.frame(times = xseq))
plot(mcycle, ylim = range(preds))
for(ii in 1:5) lines(xseq, preds[ , ii], col = 2)

# avoiding modelling the variance
fitCycleConst <- qgam(accel ~ s(times, k = 20, bs = "ad"),
                      data = mcycle, qu = 0.9)
plot(fitCycleConst)

xseq <- with(mcycle, seq(min(times), max(times), length.out = 100))
preds <-predict(fitCycleConst,newdata = data.frame(times = xseq))
preds_var <-predict(fitCycle1,newdata = data.frame(times = xseq))

plot(mcycle, ylim = range(preds))
lines(xseq, preds, col = 2)
lines(xseq, preds_var, col = 3)
%>% 