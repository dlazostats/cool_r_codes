# 1. Cargar las librerías necesarias
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)

# 2. Crear el dataframe con tus datos
df <- data.frame(
  fecha = as.Date(c(
    "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01",
    "2022-07-01", "2022-08-01", "2022-09-01", "2022-10-01", "2022-11-01", "2022-12-01",
    "2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01", "2023-05-01", "2023-06-01",
    "2023-07-01", "2023-08-01", "2023-09-01", "2023-10-01", "2023-11-01", "2023-12-01",
    "2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01",
    "2024-07-01", "2024-08-01", "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01",
    "2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01", "2025-05-01", "2025-06-01",
    "2025-07-01", "2025-08-01", "2025-09-01", "2025-10-01", "2025-11-01", "2025-12-01",
    "2026-01-01", "2026-02-01", "2026-03-01", "2026-04-01", "2026-05-01", "2026-06-01",
    "2026-07-01"
  )),
  consumo = c(
    154215.11, 120991.54, 127679.25, 145376.00, 124507.30, 163066.74, 148557.06,
    165268.21, 176036.41, 158016.07, 117676.00, 141189.70, 89472.03, 114418.00,
    108865.40, 101612.20, 118441.00, 76163.00, 65780.00, 87637.18, 74293.00,
    55137.00, 48907.00, 55606.00, 73028.00, 71239.00, 67667.00, 83269.00,
    68107.04, 68213.00, 56888.00, 74193.94, 86071.00, 90388.35, 63813.39,
    36367.00, 66914.00, 89528.32, 100535.01, 71702.96, 129396.45, 124869.10,
    70948.94, 86196.04, 86155.90, 83970.00, 103989.16, 139277.97, 129987.40,
    134210.30, 107530.39, 160176.82, 153453.63, 163936.22, 230983.19
  )
)

# 3. Convertir el dataframe a un tsibble con frecuencia mensual
ts_data <- df %>%
  mutate(fecha = yearmonth(fecha)) %>%
  as_tsibble(index = fecha)

# 4. Descomposición con STL
dcmp <- ts_data %>%
  model(
    stl = STL(consumo ~ trend(window = 13) + season(window = "periodic"))
  )

# 5. Graficar los componentes descompuestos (Tendencia, Estacionalidad, Residuo)
dcmp %>%
  components() %>%
  autoplot()

ts_data %>%
  gg_season(consumo, labels = "both")+
  theme_bw()

# Autocorrelación simple y parcial
ts_data %>%
  ACF(consumo, lag_max = 24) %>%
  autoplot()

ts_data %>%
  ACF(difference(consumo), lag_max = 24) %>%
  autoplot() +
  labs(title = "ACF de la Primera Diferencia del Consumo")

ts_data %>%
  gg_tsdisplay(difference(consumo), plot_type = "auto")

# O el gráfico combinado completo
ts_data %>%
  gg_tsdisplay(consumo, plot_type = "auto")

# Calcular la fuerza de la tendencia y de la estacionalidad
ts_data %>%
  features(consumo, feat_stl)

# Prueba KPSS para evaluar si la serie es estacionaria
ts_data %>%
  features(consumo, unitroot_kpss)

# Sugerencia de cuántas diferencias se necesitan (d)
ts_data %>%
  features(consumo, unitroot_ndiffs)

# Sugerencia de cuántas diferencias estacionales se necesitan (D)
ts_data %>%
  features(consumo, unitroot_nsdiffs)

# Inspeccionar residuos del STL
dcmp %>%
  components() %>%
  gg_tsdisplay(remainder, plot_type = "histogram")


# arima model
#............
ts_data <- df %>%
  mutate(fecha = yearmonth(fecha)) %>%
  as_tsibble(index = fecha)

fit <- ts_data %>%
  model(
    arima_auto = ARIMA(consumo)
  )

# Ver el orden del modelo seleccionado automáticamente (p, d, q)(P, D, Q)[12]
fit %>% report()
fc <- fit %>%
  forecast(h = "6 months")
fc %>%
  autoplot(ts_data) +
  labs(
    title = "Pronóstico del Consumo para los próximos 6 meses (ARIMA)",
    x = "Fecha",
    y = "Consumo"
  )

fc %>%
  hilo(level = c(80, 95)) 

fit %>%
  gg_tsresiduals()

fit %>% 
  accuracy()

# 1. Crear la estructura de Backtesting (Ventana expansiva)
# Por ejemplo: Empezar con al menos 24 meses de entrenamiento y avanzar de 1 en 1 mes
ts_cv <- ts_data %>%
  stretch_tsibble(.init = 24, .step = 1)

# 2. Re-entrenar el modelo en cada una de las ventanas del backtesting
fit_cv <- ts_cv %>%
  model(arima = ARIMA(consumo))

# 3. Generar pronósticos a h pasos (ej. h = 1 mes hacia adelante)
fc_cv <- fit_cv %>%
  forecast(h = 1)

# 4. Calcular el error de validación cruzada / backtesting
fc_cv %>%
  accuracy(ts_data)

## ETS model
#----------
# 1. Ajustar el modelo ETS automático
fit_ets <- ts_data %>%
  model(ets_auto = ETS(consumo))

# Ver los componentes seleccionados (Error, Tendencia, Estacionalidad)
fit_ets %>% report()

# 2. Calcular las métricas de error de entrenamiento (In-Sample)
fit_ets %>% accuracy()

# 3. Realizar Backtesting (Time Series Cross-Validation)
ts_cv <- ts_data %>%
  stretch_tsibble(.init = 24, .step = 1)

fc_ets_cv <- ts_cv %>%
  model(ets_auto = ETS(consumo)) %>%
  forecast(h = 1)

# Ver el error de validación cruzada
fc_ets_cv %>% accuracy(ts_data)

fit_ets %>%
  forecast(h = "6 months")%>%
  autoplot(ts_data)

## Other model
#--------------
fit_oth <- ts_data %>%
  model(mld_out = NNETAR(consumo))

# Ver los componentes seleccionados (Error, Tendencia, Estacionalidad)
fit_oth %>% report()

# 2. Calcular las métricas de error de entrenamiento (In-Sample)
fit_oth %>% accuracy()

# 3. Realizar Backtesting (Time Series Cross-Validation)
ts_cv <- ts_data %>%
  stretch_tsibble(.init = 24, .step = 1)

fc_ets_cv <- ts_cv %>%
  model(ets_auto = NNETAR(consumo)) %>%
  forecast(h = 1)

# Ver el error de validación cruzada
fc_ets_cv %>% accuracy(ts_data)

fit_oth %>%
  forecast(h = "6 months")%>%
  autoplot(ts_data)


# croston
#-----------
fit_oth <- ts_data %>%
  model(mld_out = CROSTON(consumo, type = "sba"))

# Ver los componentes seleccionados (Error, Tendencia, Estacionalidad)
fit_oth %>% report()

# 2. Calcular las métricas de error de entrenamiento (In-Sample)
fit_oth %>% accuracy()

# 3. Realizar Backtesting (Time Series Cross-Validation)
ts_cv <- ts_data %>%
  stretch_tsibble(.init = 24, .step = 1)

fc_ets_cv <- ts_cv %>%
  model(ets_auto = CROSTON(consumo, type = "sba")) %>%
  forecast(h = 1)

# Ver el error de validación cruzada
fc_ets_cv %>% accuracy(ts_data)

fit_oth %>%
  forecast(h = "6 months")%>%
  autoplot(ts_data)

