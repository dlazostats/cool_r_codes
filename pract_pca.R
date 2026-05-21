library(dplyr)
library(ggplot2)
library(ggrepel)   
library(factoextra)

# set working directory
script_name <- 'pract_pca.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

# load data
db0<-read.csv("dftrain.csv")
set.seed(2121)
db0_sqmple<-db0 %>% 
            slice_sample(n = 1500)%>% 
            dplyr::select(C:Pb)

## PCA
pca <- prcomp(db0_sqmple, center = TRUE, scale. = TRUE)
pca$sdev        # std deviations of each PC (sqrt of eigenvalues)
pca$rotation    # loadings matrix  (p × p):  variables × PCs
pca$x           # scores matrix   (n × p):  observations × PCs

eigenvalues  <- pca$sdev^2
prop_var     <- eigenvalues/sum(eigenvalues)
cum_var      <- cumsum(prop_var)
var_df <- data.frame(
  PC       = factor(paste0("PC", seq_along(eigenvalues)),
                    levels = paste0("PC", seq_along(eigenvalues))),
  eigenvalue = eigenvalues,
  prop_var = prop_var,
  cum_var  = cum_var
)
print(var_df, digits = 3)


# Kaiser criterion: keep PCs with eigenvalue > 1
as.character(var_df[var_df$eigenvalue > 1, "PC"])

# Scree plot
ggplot(var_df, aes(x = PC, y = eigenvalue, group = 1)) +
  geom_col(fill = "#7F77DD", alpha = 0.7) +
  geom_line(colour = "#534AB7", linewidth = 0.8) +
  geom_point(colour = "#534AB7", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 4.2, y = 1.08, label = "Kaiser threshold", size = 3,
           colour = "grey50", hjust = 1) +
  labs(title = "Scree plot — Chemical COmposition",
       y = "Eigenvalue", x = NULL) +
  theme_minimal(base_size = 13)

# Cumulative variance plot
ggplot(var_df, aes(x = PC, y = cum_var * 100, group = 1)) +
  geom_line(colour = "#1D9E75", linewidth = 1) +
  geom_point(colour = "#1D9E75", size = 3) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  labs(title = "Cumulative variance explained",
       y = "Cumulative %", x = NULL) +
  theme_minimal(base_size = 13)

## Loadings - what do the PCs mean
loadings_df <- as.data.frame(pca$rotation)
print(round(loadings_df, 3))
# Rule of thumb: |loading| > 0.3 is meaningful for n=50, p=4

# Heatmap of loadings
loadings_long <- reshape2::melt(as.matrix(pca$rotation))
colnames(loadings_long) <- c("Variable", "PC", "Loading")

ggplot(loadings_long, aes(x = PC, y = Variable, fill = Loading)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Loading, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#3B8BD4", mid = "white", high = "#D85A30",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "PCA loadings heatmap") +
  theme_minimal(base_size = 13)

fviz_cos2(pca, choice = "var", axes = 1:2) + #the fraction of a variable's total variance captured by the PC1+PC2 plane
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 0.2, linetype = "dashed", colour = "green")

fviz_cos2(pca, choice = "var", axes = 1:5)

# Score plots
#-------------
scores_df <- as.data.frame(pca$x)
scores_df$state <- rownames(scores_df)

# Percent variance for axis labels
pct <- round(prop_var * 100, 1)

ggplot(scores_df, aes(x = PC1, y = PC2, label = state)) +
  geom_point(colour = "#534AB7", alpha = 0.8, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  labs(
    title  = "PCA scores — Chemical Composition",
    x      = paste0("PC1 (", pct[1], "% variance)"),
    y      = paste0("PC2 (", pct[2], "% variance)")
  ) +
  theme_minimal(base_size = 13)

# Colouring the points by a known categorical label (grade, supplier, production batch) 
# would immediately reveal whether PCA has separated groups you care about, or whether 
# the variation is just noise

# --- 6a. Base R biplot (fastest) ---
scale_factor <- max(abs(scores_df[, c("PC1", "PC2")])) /
  max(abs(pca$rotation[, c("PC1", "PC2")])) * 0.7
loadings_scaled <- as.data.frame(pca$rotation * scale_factor)
loadings_scaled$variable <- rownames(loadings_scaled)
ggplot(scores_df, aes(x = PC1, y = PC2)) +
  geom_point(colour = "#534AB7", alpha = 0.3, size = 2) +
  #geom_text_repel(aes(label = state), size = 2.5, colour = "grey40",
  #                max.overlaps = 15) +
  # Loading arrows
  geom_segment(data = loadings_scaled,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "#D85A30", linewidth = 0.8) +
  geom_text_repel(data = loadings_scaled,
                  aes(x = PC1, y = PC2, label = variable),
                  colour = "#D85A30", size = 3.5, fontface = "bold",
                  max.overlaps = 10) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
  labs(
    title    = "Biplot — USArrests PCA",
    subtitle = "Blue = states (scores)  |  Red = variables (loadings)",
    x        = paste0("PC1 (", pct[1], "%)"),
    y        = paste0("PC2 (", pct[2], "%)"),
    caption  = "Loadings scaled for visual alignment"
  ) +
  theme_minimal(base_size = 13)


# Using FactoExtra
#-------------------
# Scree (with broken-stick reference)
fviz_eig(pca, addlabels = TRUE, ncp = 4)

# Variable contributions to PC1 and PC2
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)

# cos2: quality of variable representation on the plane
fviz_cos2(pca, choice = "var", axes = 1:2)


# RECONSTRUCTION ERROR — how much info do you lose at k PCs?
# -----------------------------------------------------------------------------
reconstruct <- function(pca_obj, k) {
  # Rebuild the standardised data from k PCs, then back-scale
  scores   <- pca_obj$x[, 1:k, drop = FALSE]
  loadings <- t(pca_obj$rotation[, 1:k, drop = FALSE])
  recon_scaled <- scores %*% loadings
  # Reverse standardisation
  sweep(sweep(recon_scaled, 2, pca_obj$scale, "*"), 2, pca_obj$center, "+")
}

original <- scale(db0_sqmple, center = pca$center, scale = pca$scale)

for (k in 1:14) {
  recon  <- reconstruct(pca, k)
  rmse   <- sqrt(mean((original - recon)^2))
  cat(sprintf("k = %d PC(s): RMSE = %.4f  |  Var explained = %.1f%%\n",
              k, rmse, cum_var[k] * 100))
}
#there is no strong low-dimensional structure in this dataset. 
#Variance is spread fairly evenly across many PCs rather than concentrated in the first few

# OUTLIER DETECTION via Mahalanobis distance in PC space
# (Equivalent to Mahalanobis distance in the original scaled space)
# ----------------------------------------------------------------------
scores_k <- pca$x[, 1:2]  # use first 2 PCs
mah_dist  <- mahalanobis(scores_k, colMeans(scores_k), cov(scores_k))
cutoff    <- qchisq(0.975, df = 2)  # chi-sq threshold, df = k

outliers  <- names(mah_dist[mah_dist > cutoff])
cat("\nPotential outliers (Mahalanobis, p < 0.025):", outliers, "\n")

# Visualise
scores_df$mahal  <- mah_dist
scores_df$outlier <- mah_dist > cutoff

ggplot(scores_df, aes(x = PC1, y = PC2, colour = outlier, label = state)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(size = 2.8, max.overlaps = 20) +
  scale_colour_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                      labels = c("Normal", "Outlier")) +
  labs(title  = "Outlier detection in PC space",
       colour = NULL,
       x      = paste0("PC1 (", pct[1], "%)"),
       y      = paste0("PC2 (", pct[2], "%)")) +
  theme_minimal(base_size = 13)


