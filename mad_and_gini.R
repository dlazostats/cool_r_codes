library(robustbase)  # For robust estimators
library(moments)     # For additional statistics
library(DescTools)
library(summarytools)
library(sjstats)
x <- c(456, 476, 470, 468, 472, 480, 478, 478, 480, 456, 464, 456, 468, 487, 480, 456, 495, 480, 469, 480, 486, 458)
dtf<-data.frame(x=x)
med<-median(x)
mad<-descr(x)[[8]]
descr(x)
dtf$absdiff<-abs(x-median(x))
median(dtf$absdiff)*1.4826

# intervalo mediano
inf_md<-med-mad
sup_md<-med+mad
test$indx<-ifelse(test$FLUENCIA>=inf_md&test$FLUENCIA<=sup_md,1,0)
mean(test$indx)
mad/(max(x)-min(x))*100
gmd(x)
mad

# 5. Rousseeuw-Croux estimators (highly robust)
qn_value <- Qn(data)
sn_value <- Sn(data)
cat("5. Qn estimator:", qn_value, "\n")
cat("6. Sn estimator:", sn_value, "\n")

# 7. Biweight midvariance (robust alternative to variance)
# Note: This gives variance-like measure, so we take sqrt for scale
bw_midvar <- sqrt(covMcd(data, cor=FALSE)$cov[1,1])
cat("7. Biweight-based scale:", bw_midvar, "\n")

cat("\n=== TRIMMED ALTERNATIVES ===\n")
# 8. Trimmed standard deviation (removes extreme 10%)
trimmed_sd <- sd(data, na.rm=TRUE)*sqrt(0.8)  # Approximate correction
cat("8. Trimmed SD (10% trimming):", trimmed_sd, "\n")

# 9. Winsorized standard deviation
xw<-Winsorize(x)
winsorized_sd <- sd(xw)

cat("\n=== RECOMMENDATIONS ===\n")
cat("• MAD is most commonly used modern alternative\n")
cat("• Qn/Sn are best for heavy-tailed distributions\n")
cat("• IQR is most interpretable for general audiences\n")
cat("• For your data, MAD (", mad, ") is probably optimal\n")
