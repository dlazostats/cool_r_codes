#GAMLSS: modelos aditivos generalizados para posición, escala y forma
#---------------------------------------------------------------------
library(dplyr)
library(gamlss)
library(tidymodels)
library(ggthemes)
library(jtools)
library(MLmetrics)

# Set working directory
script_name <- 'gamlss.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# Loading Data 
db0<-read.csv("databaco_fluenc.csv")
db0$dim_prom<-round(db0$dim_prom,2)

# EDA
db0 %>% mutate(fluencia="fluenc") %>% 
  ggplot(aes(x = fluencia, y = fluenc)) + 
  geom_point(size = 1.5,
             alpha = .2,
             position = position_jitter(
              seed = 1, width = .1
              )) + 
  theme_hc()+ coord_flip()

db0 %>% mutate(dimcat=as.factor(db0$dim_prom)) %>% 
ggplot(aes(dimcat, fluenc)) + 
  geom_point(position = position_jitter(seed = 1,
                                        width = .1)) + 
  geom_point(stat = "summary", 
             fun = "mean", colour = "red", size = 4)

db0 %>% mutate(fluencia="fluenc") %>% 
  ggplot(aes(x = fluencia, y = fluenc)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  theme_hc()+ coord_flip()

db0 %>% mutate(fluencia="fluenc") %>% 
  ggplot() +
  aes(x = fluencia,
      y = fluenc) +
  geom_flat_violin(position = position_nudge(x = .2),
                   alpha = 0.7) +
  geom_point(position = position_jitter(w = .15)) +
  geom_boxplot(width = .25) + # New code added here
  coord_flip() +theme_hc()

# Train-test sets
set.seed(123)
split <- initial_split(db0, strata = fluenc)
train <- training(split)
test <- testing(split)

# Modelos
#=========
## Lineales Generalizados
#--------------------------
## Linear basico
l1<-lm(fluenc~.,data=train)
l1 %>% summ()

modelo_lm <- gamlss(formula = fluenc ~ .,
                    family  = NO,
                    data    = train,
                    trace   = FALSE)
modelo_lm
plot(modelo_lm)
wp(modelo_lm, ylim.all = 1)

# usando distribuccion gamma debido a que es positivo y sesgado
modelo_glm <- gamlss(formula = fluenc ~ .,
                    family  = GA,
                    data    = train,
                    trace   = FALSE)
modelo_glm
plot(modelo_glm)
wp(modelo_glm, ylim.all = 1)

## Generalizados Aditivo
#------------------------
form<-as.formula(paste0("fluenc~",
                        paste0("pb(",
                               names(db0 %>% select(-fluenc)),")",
                               collapse="+")))
modelo_gam <- gamlss(formula = form,
                    family  = GA,
                    data    = train,
                    trace   = FALSE)
plot(modelo_gam)
wp(modelo_gam, ylim.all = 1)

## comparing models 
GAIC(modelo_lm, modelo_glm, modelo_gam)
plm<-predict(modelo_lm,train,what="mu")
pglm<-predict(modelo_glm,train,what="mu")
pgam<-predict(modelo_gam,test,what="mu")
RMSE(plm,train$fluenc)
RMSE(pglm,train$fluenc)
RMSE(pglm,train$fluenc)

## Comparando Distribuciones
#---------------------------
distribuciones <- fitDist(
  db0$fluenc,
  k = 2, # esta penalización equivale al AIC
  type = "realplus",
  trace = FALSE,
  try.gamlss = TRUE
)
