# Example GAM
#------------
library(mgcv)
library(tidymodels)
library(visdat)
library(tidyr)
library(dplyr)
library(DataExplorer)
library(SmartEDA)
library(summarytools)

# Example 1
## TidyGam
g <- gam(mpg ~ s(hp) + am + qsec, data = mtcars)
summary(g)
tidy(g)
tidy(g, parametric = TRUE)
glance(g)
augment(g) %>% as.data.frame() %>% head()

# Example 2
pisa = read.csv('https://raw.githubusercontent.com/m-clark/generalized-additive-models/master/data/pisasci2006.csv')
head(pisa)

## EDA
pisa_q<-pisa%>% select(-Country)
vis_value(pisa_q)
vis_cor(pisa_q)
vis_miss(pisa_q)
data_vis_dat(pisa_q)
plot_intro(pisa_q)
plot_missing(pisa_q)
ExpData(pisa_q)
ExpNumStat(pisa_q, round = 2)
ExpNumViz(pisa_q, target = 'Overall')
dfSummary(pisa_q,style="grid")

pisa_q %>% 
  select(Overall,Edu,HDI,Health,Income,Interest,Support) %>% 
  gather(var,value,-Overall) %>% 
  ggplot(aes(x=value,y=Overall))+
    geom_point(color="grey")+
    geom_smooth(se=F,color="steelblue")+
    facet_wrap(~var,scale="free")+
    theme_minimal()

# Simple gam
mod_lm <- gam(Overall ~ Income, data = pisa)
summary(mod_lm)
summary(lm(Overall ~ Income, data = pisa))

mdl_cr<-gam(Overall~s(Income,bs="cr"),data=pisa)
summary(mdl_cr)
