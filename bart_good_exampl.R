# ============================================================
#  BART en R — Ejemplo completo con datos simulados
#  Paquete: BART (Chipman, George & McCulloch)
# ============================================================

# install.packages("BART")
library(BART)
library(ggplot2)

# ------------------------------------------------------------
# 1. DATOS SIMULADOS
#    Función verdadera: f(x) = sin(2πx) + 0.5·x²
#    Con ruido gaussiano σ = 0.3
# ------------------------------------------------------------
set.seed(42)
n_train <- 150
n_test  <- 80

x_train <- sort(runif(n_train, 0, 1))
y_train <- sin(2 * pi * x_train) + 0.5 * x_train^2 + rnorm(n_train, sd = 0.3)

x_test  <- sort(runif(n_test, 0, 1))
y_true  <- sin(2 * pi * x_test) + 0.5 * x_test^2   # sin ruido, para comparar

# BART espera matrices, no vectores
X_train <- matrix(x_train, ncol = 1)
X_test  <- matrix(x_test,  ncol = 1)


# ------------------------------------------------------------
# 2. AJUSTAR BART
#    Argumentos clave:
#      x.train  — matriz de predictores (entrenamiento)
#      y.train  — vector de respuesta
#      x.test   — matriz de predictores (predicción)
#      ntree    — número de árboles (default 200)
#      ndpost   — muestras post burn-in
#      nskip    — iteraciones de burn-in
#      k        — controla el prior de las hojas (default 2)
#      power    — β del prior de profundidad (default 2)
#      base     — α del prior de profundidad (default 0.95)
# ------------------------------------------------------------
bart_fit <- wbart(
  x.train = X_train,
  y.train = y_train,
  x.test  = X_test,
  ntree   = 200,       # más árboles = más estable, más lento
  ndpost  = 1000,      # 1000 muestras de la posterior
  nskip   = 250,       # descartar las primeras 250 (burn-in)
  k       = 2,
  power   = 2,
  base    = 0.95,
  printevery = 500     # progreso cada 500 iteraciones
)

# ------------------------------------------------------------
# 3. EXTRAER RESULTADOS
#    bart_fit$yhat.test        → matriz (ndpost × n_test)
#                                cada fila = 1 muestra posterior
#    bart_fit$yhat.test.mean   → media posterior por punto
# ------------------------------------------------------------
pred_mean <- bart_fit$yhat.test.mean

# Intervalos creíbles al 95%
pred_lower <- apply(bart_fit$yhat.test, 2, quantile, 0.025)
pred_upper <- apply(bart_fit$yhat.test, 2, quantile, 0.975)

# Desviación estándar posterior (incertidumbre)
pred_sd    <- apply(bart_fit$yhat.test, 2, sd)

cat("RMSE en test:", round(sqrt(mean((pred_mean - y_true)^2)), 4), "\n")
cat("Cobertura IC 95%:", round(mean(y_true >= pred_lower & y_true <= pred_upper), 3), "\n")


# ------------------------------------------------------------
# 4. GRÁFICO 1 — Predicciones + intervalo creíble
# ------------------------------------------------------------
df_plot <- data.frame(
  x      = x_test,
  verdad = y_true,
  media  = pred_mean,
  lower  = pred_lower,
  upper  = pred_upper
)

ggplot(df_plot, aes(x = x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#7F77DD", alpha = 0.25) +
  geom_line(aes(y = media),  color = "#534AB7", linewidth = 1) +
  geom_line(aes(y = verdad), color = "#1D9E75", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = data.frame(x = x_train, y = y_train),
             aes(x = x, y = y), color = "#888780", size = 1.2, alpha = 0.5) +
  labs(
    title    = "BART — Predicción vs. función verdadera",
    subtitle = "Banda morada = IC 95% | Verde = f(x) verdadera | Gris = datos de entrenamiento",
    x = "x", y = "y"
  ) +
  theme_minimal(base_size = 13)


# ------------------------------------------------------------
# 5. GRÁFICO 2 — Incertidumbre a lo largo de x
#    Donde hay pocos datos → banda más ancha
# ------------------------------------------------------------
df_sd <- data.frame(x = x_test, sd = pred_sd)

ggplot(df_sd, aes(x = x, y = sd)) +
  geom_line(color = "#E85D24", linewidth = 1) +
  geom_hline(yintercept = mean(pred_sd), linetype = "dashed",
             color = "#888780", linewidth = 0.6) +
  labs(
    title    = "Incertidumbre posterior por región",
    subtitle = "Desviación estándar de las muestras MCMC",
    x = "x", y = "SD posterior"
  ) +
  theme_minimal(base_size = 13)


# ------------------------------------------------------------
# 6. GRÁFICO 3 — Convergencia del sampler (traceplot)
#    Verificar que la cadena de Markov mezcló bien
# ------------------------------------------------------------
sigma_samples <- bart_fit$sigma   # evolución de σ en cada iteración

df_trace <- data.frame(
  iter  = seq_along(sigma_samples),
  sigma = sigma_samples
)

ggplot(df_trace, aes(x = iter, y = sigma)) +
  geom_line(color = "#3B8BD4", linewidth = 0.4, alpha = 0.8) +
  geom_vline(xintercept = 250, linetype = "dashed",
             color = "#E85D24", linewidth = 0.7) +
  annotate("text", x = 270, y = max(sigma_samples) * 0.98,
           label = "fin burn-in", color = "#E85D24", size = 3.5, hjust = 0) +
  labs(
    title    = "Traceplot de σ — convergencia del sampler",
    subtitle = "La cadena debe estabilizarse tras el burn-in",
    x = "Iteración", y = "σ estimado"
  ) +
  theme_minimal(base_size = 13)


# ------------------------------------------------------------
# 7. IMPORTANCIA DE VARIABLES (múltiples predictores)
#    bart_fit$varcount → cuántas veces se usó cada variable
#    en los splits de cada iteración
# ------------------------------------------------------------

# Ejemplo con 3 predictores:
set.seed(7)
n2 <- 200
X2_train <- matrix(runif(n2 * 3), ncol = 3,
                   dimnames = list(NULL, c("edad", "ingreso", "ruido")))
y2_train  <- 3 * X2_train[,"edad"] + 2 * sin(X2_train[,"ingreso"] * pi) +
  rnorm(n2, sd = 0.5)

X2_test  <- matrix(runif(50 * 3), ncol = 3,
                   dimnames = list(NULL, c("edad", "ingreso", "ruido")))

bart_multi <- wbart(
  x.train = X2_train, y.train = y2_train,
  x.test  = X2_test,
  ntree = 100, ndpost = 500, nskip = 100,
  printevery = 0
)

# Proporción de splits por variable (promedio sobre muestras)
var_importance <- colMeans(bart_multi$varcount) /
  sum(colMeans(bart_multi$varcount))

df_imp <- data.frame(
  variable   = names(var_importance),
  importancia = var_importance
)

ggplot(df_imp, aes(x = reorder(variable, importancia), y = importancia)) +
  geom_col(fill = "#7F77DD", alpha = 0.85, width = 0.5) +
  geom_text(aes(label = scales::percent(importancia, accuracy = 0.1)),
            hjust = -0.2, size = 3.5, color = "var(--color-text-primary)") +
  coord_flip() +
  labs(
    title    = "Importancia de variables en BART",
    subtitle = "Proporción de splits en los que se usó cada variable",
    x = NULL, y = "Proporción de splits"
  ) +
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent) +
  theme_minimal(base_size = 13)