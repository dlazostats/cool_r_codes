library(tsfknn)

# ── Basic forecast ──────────────────────────────────────────────────────────
pred <- knn_forecasting(USAccDeaths,
                        h    = 12,       # forecast horizon
                        lags = 1:12,     # lag features (full seasonal cycle)
                        k    = 3,        # neighbors
                        msas = "MIMO",   # better for multi-step
                        cf   = "weighted") # weight closer neighbors more

pred$prediction

# ── Inspect which neighbors were used ──────────────────────────────────────
nn <- nearest_neighbors(pred)
print(nn)

# ── Visualize forecast + neighbors ─────────────────────────────────────────
library(ggplot2)
autoplot(pred, highlight = "neighbors")

# ── Proper accuracy evaluation with rolling origin ──────────────────────────
ro <- rolling_origin(pred, h=12)

# RMSE and MAE across all rolling windows
ro$global_accu

# ── Try multiple K values at once (ensemble) ────────────────────────────────
pred_ensemble <- knn_forecasting(USAccDeaths,
                                 h    = 12,
                                 lags = 1:12,
                                 k    = c(2, 4, 6))  # averages all three
pred_ensemble$prediction

# ── Automatic mode (let the package choose k and lags) ─────────────────────
pred_auto <- knn_forecasting(USAccDeaths, h=12)
summary(pred_auto)
autoplot(pred_auto, highlight = "neighbors")
