#GAMLSS - Generalized Additive Models for Location, Scale and Shape 
#===================================================================
## loading libraries
library(dplyr)
library(gamlss)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(skimr)
library(gamlss.util)
library(colorspace)
library(SmartEDA)
library(gamlss.add)

#loading data
data("rent")
datos <- rent
datos <- datos %>% select(R, Fl, A, H, loc)
colnames(datos) <- c("precio", "metros", "anyo", "calefaccion", "situacion")

# EDA
head(datos)
ExpData(datos)
skim(datos)

#distribuicon precios
ggplot(data = datos, aes(x = precio)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribución del precio de los pisos") +
  theme_bw()

# GAMLSS
#-------
# 1.-regresion lineal
# El resultado es igual al obtenido con lm()
modelo_lm <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = NO,
  data    = datos,
  trace   = FALSE
 )
summary(modelo_lm)
exp(5.73165) # para estimar la varianza equivalente a residual standar error
mlnom<-lm(precio ~ metros + anyo + calefaccion + situacion,datos)
summary(mlnom)
sqrt(sum(c(datos$precio-predict(mlnom))^2)/length(datos$precio))
plot(modelo_lm)
wp(modelo_lm, ylim.all = 1)
wp(modelo_lm)

# 2.-GLM : generalized linear model
GA()
show.link(family = GA)
modelo_gama <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
 )
summary(modelo_gama)
plot(modelo_gama)
wp(modelo_gama)

GAIC(modelo_lm, modelo_gama) #comparar modelos

# 3.- GAM : generalized additive linear model (pb - spline)
modelo_gam <- gamlss(
  formula = precio ~ pb(metros) + pb(anyo) + calefaccion + situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)
summary(modelo_gam)

#comparar aporte de los predictores, al medir impacto al eliminar cada uno
drop1(modelo_gam, parallel = "multicore", ncpus = 4)   
drop1(modelo_lm)

# impacto de cada variable con log(y)
term.plot(modelo_gam, pages = 1, ask = FALSE, rug = TRUE)
plot(modelo_gam)
wp(modelo_gam)

GAIC(modelo_lm, modelo_gama, modelo_gam)

# 4.- GAMLSS 
modelo_gamlss <- gamlss(
  formula = precio ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  sigma.formula = ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)
summary(modelo_gamlss)
term.plot(modelo_gamlss, parameter = "mu",pages = 1, ask = FALSE, rug = TRUE)
term.plot(modelo_gamlss, parameter = "sigma",pages = 1, ask = FALSE, rug = TRUE)
drop1(modelo_gamlss, parameter = "mu", parallel = "multicore", ncpus = 4)
drop1(modelo_gamlss, parameter = "sigma", parallel = "multicore", ncpus = 4)

wp(modelo_gamlss)
GAIC(modelo_lm, modelo_gama,modelo_gam,modelo_gamlss) #comparar modelos

# Modelo GAMLSS para distribución Box-Cox Cole-Green con parámetros de media,
# escala y forma. Se emplea P-splines para los predictores continuos
BCCGo() #distribucion de tres parametros
modelo_gamlss_2 <- gamlss(
  formula = precio ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  sigma.formula = ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  nu.formula    = ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  family  = BCCGo,
  data    = datos,
  trace   = FALSE
)
summary(modelo_gamlss_2)
term.plot(modelo_gamlss_2, parameter = "nu", pages = 1, ask = FALSE, rug = TRUE)
GAIC(modelo_lm, modelo_gama,modelo_gam,modelo_gamlss,modelo_gamlss_2) #comparar modelos
drop1(modelo_gamlss_2, parameter = "nu", parallel = "multicore", ncpus = 4)
# solo calefaccion aporta

modelo_gamlss_2 <- gamlss(
  formula = precio ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  sigma.formula = ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  nu.formula    = ~ calefaccion,
  family  = BCCGo,
  data    = datos,
  trace   = FALSE
)
summary(modelo_gamlss_2)
GAIC(modelo_lm, modelo_gama,modelo_gam,modelo_gamlss,modelo_gamlss_2) #comparar modelos

# Determinar mejor distribucion 
distribuciones <- fitDist(
  datos$precio,
  k = 2, # esta penalización equivale al AIC
  type = "realplus",
  trace = FALSE,
  try.gamlss = TRUE
)
summary(distribuciones)
distribuciones$fits %>%
  enframe(name = "distribucion", value = "GAIC") %>%
  arrange(GAIC)

# simulacion vs real
simulacion <- rBCCG(
  n  = 2000,
  mu = 749.2743664,
  sigma = exp(-0.7520409),
  nu = 0.2530936
)
ggplot() +
  geom_histogram(data = datos,aes(x = precio),alpha = 0.5, fill = "gray50") +
  geom_histogram(data = data.frame(simulacion),aes(x = simulacion),alpha = 0.3, fill = "firebrick") +
  labs(title = "Distribución real vs Distribución simulada BCCG") +
  theme_bw()

## Funciones no lineales
#-------------------------
data("film90")
datos <- film90
datos <- film90 %>%dplyr::select(log_recaudacion = lborev1, log_proyecciones =  lboopen)
head(datos)

ggplot(data = datos, aes(x = log_proyecciones, y = log_recaudacion)) +
  geom_point(alpha = 0.3)  +
  labs(title = "Recaudación vs Número de proyecciones") +
  theme_bw()

## pslines
modelo_ps <- gamlss(
  formula = log_recaudacion ~ pb(log_proyecciones),
  data    = datos,
  family  = NO,
  trace   = FALSE
)
summary(modelo_ps)
datos <- datos %>%
  mutate(prediccion_ps = fitted(modelo_ps))
ggplot(
  data = datos,
  aes(x = log_proyecciones, y = log_recaudacion)) +
  geom_point(alpha = 0.2)  +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_ps, color = "P-splines"),
    size = 1) +
  scale_color_manual(
    breaks = c("P-splines"),
    values = c("P-splines" = "red")) +
  labs(title = "Recaudación vs Número de proyecciones") +
  theme_bw() +
  theme(legend.position = "bottom")
getPEF(
  modelo_ps,
  term      = "log_proyecciones",
  parameter = "mu",
  plot      = TRUE
)
drop1(object = modelo_ps, parallel = "multicore", ncpus = 4)

## cubic splines
modelo_cs <- gamlss(
  formula = log_recaudacion ~ cs(log_proyecciones, df = 5),
  data    = datos,
  family  = NO,
  trace   = FALSE
)

summary(modelo_cs)
datos <- datos %>%
  mutate(prediccion_cs = fitted(modelo_cs))

ggplot(
  data = datos,
  aes(x = log_proyecciones, y = log_recaudacion)) +
  geom_point(alpha = 0.2)  +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_ps, color = "P-splines"),
    size = 1) +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_cs, color = "Cubic-splines"),
    size = 1) +
  scale_color_manual(
    breaks = c("P-splines", "Cubic-splines"),
    values = c("P-splines" = "red", "Cubic-splines" = "blue")) +
  labs(title = "Recaudación vs Número de proyecciones") +
  theme_bw() +
  theme(legend.position = "bottom")

# analisis de aporte de variable mediante dependencia parcial
getPEF(
  modelo_cs,
  term      = "log_proyecciones",
  parameter = "mu",
  plot      = TRUE
)
drop1(object = modelo_ps, parallel = "multicore", ncpus = 4)

## redes neuronales 
modelo_nn <- gamlss(
  formula = log_recaudacion ~ nn(~log_proyecciones, size = 20, decay = 0.1),
  data    = datos,
  family  = NO,
  trace   = FALSE
)
datos <- datos %>%
  mutate(prediccion_nn = fitted(modelo_nn))
ggplot(
  data = datos,
  aes(x = log_proyecciones, y = log_recaudacion)) +
  geom_point(alpha = 0.2)  +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_ps, color = "P-splines"),
    size = 1) +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_cs, color = "Cubic-splines"),
    size = 1) +
  geom_line(
    aes(x = log_proyecciones, y = prediccion_nn, color = "red-neuronal"),
    size = 0.7) +
  scale_color_manual(
    breaks = c("P-splines", "Cubic-splines", "red-neuronal"),
    values = c("P-splines"="red", "Cubic-splines"="blue", "red-neuronal"="green")) +
  labs(title = "Recaudación vs Número de proyecciones") +
  theme_bw() +
  theme(legend.position = "bottom")

# distribucion univariada
modelo_gamlss <- gamlss(
  formula = log_recaudacion ~ pb(log_proyecciones),
  sigma.formula = log_recaudacion ~ pb(log_proyecciones),
  data    = datos,
  family  = NO,
  trace   = FALSE
)

plotSimpleGamlss(
  y = log_recaudacion,
  x = log_proyecciones,
  model = modelo_gamlss,
  data  = datos,
  x.val = seq(6, 16, 2),
  val   = 5,
  N     =  1000,
  ylim  = c(0,25),
  cols  = heat_hcl(100)
)

## Inferencia y prediccion
#-------------------------
modelo_gamlss <- gamlss(
  formula = log_recaudacion ~ log_proyecciones,
  sigma.formula = log_recaudacion ~ log_proyecciones,
  data    = datos,
  family  = NO,
  trace   = FALSE
)
summary(modelo_gamlss)
vcov(modelo_gamlss, type = "se") #erroes standar normales
rvcov(modelo_gamlss, type = "se") #erroes standar robustos

# intervalos de confianza
confint(
  modelo_gamlss,
  what   = c("mu"),
  level  = 0.95,
  robust = TRUE
)

confint(
  modelo_gamlss,
  what   = c("sigma"),
  level  = 0.95,
  robust = TRUE
)

#prediccion
predict(
  object  = modelo_gamlss,
  what    = "mu",
  type    = "link", # en logaritmo
  newdata = data.frame(log_proyecciones = 11)
)
predict(
  object  = modelo_gamlss,
  what    = "mu",
  type    = "response", # en unidad original
  newdata = data.frame(log_proyecciones = 11)
)
predict(
  object  = modelo_gamlss,
  what    = "mu",
  type    = "terms", # muestra la contribucion de cada termino del modelo
  newdata = data.frame(log_proyecciones = 11)
)
predict(
  object  = modelo_gamlss,
  what    = "sigma",
  type    = "link",
  newdata = data.frame(log_proyecciones = 11)
)
predict(
  object  = modelo_gamlss,
  what    = "sigma",
  type    = "response",
  newdata = data.frame(log_proyecciones = 11)
)
predictAll(
  object  = modelo_gamlss,
  type    = "response",
  newdata = data.frame(log_proyecciones = 11)
)

# seleccion de modelo
# Modelos candidatos
data("rent")
datos <- rent
datos <- datos %>% select(R, Fl, A, H, loc)
# Se renombran las variables para que sean más explicativas
colnames(datos) <- c("precio", "metros", "anyo", "calefaccion", "situacion")

modelo_1 <- gamlss(formula = precio ~ metros, 
                   family = NO, data = datos, trace = FALSE)
modelo_2 <- gamlss(formula = precio ~ metros + anyo,
                   family = NO, data = datos, trace = FALSE)
modelo_3 <- gamlss(formula = precio ~ metros,
                   family = GA, data = datos, trace = FALSE)
modelo_4 <- gamlss(formula = precio ~ metros + anyo,
                   family = GA, data = datos, trace = FALSE)
GAIC(modelo_1, modelo_2, modelo_3, modelo_4, k = 3)

# cross-validation
cv_modelo_1 <- gamlssCV(formula = precio ~ metros, family = NO, data = datos,
                        K.fold = 10, parallel = "multicore", ncpus = 4,
                        set.seed = 1)
cv_modelo_2 <- gamlssCV(formula = precio ~ metros + anyo, family = NO, data = datos,
                        K.fold = 10, parallel = "multicore", ncpus = 4,
                        set.seed = 1)
cv_modelo_3 <- gamlssCV(formula = precio ~ metros, family = GA, data = datos,
                        K.fold = 10, parallel = "multicore", ncpus = 4,
                        set.seed = 1)
cv_modelo_4 <- gamlssCV(formula = precio ~ metros + anyo, family = GA, data = datos,
                        K.fold = 10, parallel = "multicore", ncpus = 4,
                        set.seed = 1)

CV(cv_modelo_1, cv_modelo_2, cv_modelo_3, cv_modelo_4)

# validacion train-test
# Partición de los datos
set.seed(123)
id_train <- sample(x = 1:nrow(datos), size = nrow(datos)*0.8, replace = FALSE)
id_test  <- (1:nrow(datos))[-id_train]
# Modelos candidatos entrenados con la partición de entrenamiento
modelo_1 <- gamlss(formula = precio ~ metros, 
                   family = NO, data = datos[id_train, ], trace = FALSE)
modelo_2 <- gamlss(formula = precio ~ metros + anyo,
                   family = NO, data = datos[id_train, ], trace = FALSE)
modelo_3 <- gamlss(formula = precio ~ metros,
                   family = GA, data = datos[id_train, ], trace = FALSE)
modelo_4 <- gamlss(formula = precio ~ metros + anyo,
                   family = GA, data = datos[id_train, ], trace = FALSE)

# Calculo métrica de test
test_modelo_1 <- getTGD(modelo_1, newdata = datos[id_test, ])
test_modelo_2 <- getTGD(modelo_2, newdata = datos[id_test, ])
test_modelo_3 <- getTGD(modelo_3, newdata = datos[id_test, ])
test_modelo_4 <- getTGD(modelo_4, newdata = datos[id_test, ])
TGD(test_modelo_1, test_modelo_2, test_modelo_3, test_modelo_4)

# GamboostLSS
library(gamboostLSS)
data(india)
datos <- india
head(datos)
modelo <- gamboostLSS(
  formula = list(
    mu = stunting ~ bbs(mage) + bbs(mbmi) + bbs(cage) + bbs(cbmi),
    sigma = stunting ~ bbs(mage) + bbs(mbmi) + bbs(cage) + bbs(cbmi)
  ),
  families = GaussianLSS(stabilization = "MAD"),
  data = india,
  control = boost_control(
    trace = FALSE,
    mstop = c(mu = 200, sigma = 100)
  )
)
summary(modelo)

# cross-valdiation to find out the number of iterations per parameter
grid <- make.grid(
  max = c(mu = 3000, sigma = 500),
  min = 50,
  length.out = 5, 
  dense_mu_grid = FALSE
)
head(grid)
ggplot(data = grid, aes(x = mu, y = sigma)) +
  geom_point() +
  labs(title = "Número de iteraciones de ajuste para mu y sigma") +
  theme_bw()
result_grid <- cvrisk(
  object = modelo,
  folds = cv(weights = model.weights(modelo), type = "kfold", B = 5),
  grid = grid
)
plot(result_grid)
mejores_hiperparametros <- mstop(result_grid)
mejores_hiperparametros

# re entrenamiento
mstop(modelo) <- mstop(result_grid)
modelo
