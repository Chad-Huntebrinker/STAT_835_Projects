#Chad Huntebrinker
#Final Project
library(readxl)
library(VGAM)
library(pROC)

#First, load the dataset
data_set <- read_excel("Heart_Disease_data.xlsx")

#Data was pretty spread out, so we needed to break them down into 3 categories instead of 5.
data_set$num3 <- cut(data_set$num,
                     breaks = c(-1, 0, 2, 4),
                     labels = c(0, 1, 2),
                     ordered_result = TRUE)

#Use the model we found last script
vglm_final_model <- vglm(num3 ~ trestbps + sex + cp + exang + oldpeak + thalach,
                     family = cumulative(parallel = TRUE),
                     data = data_set)

#And we will also try parallel = FALSE to cover our bases
vglm_final_model_nonparallel <- vglm(num3 ~ trestbps + sex + cp + exang + oldpeak + thalach,
                         family = cumulative(parallel = TRUE),
                         data = data_set)
summary(vglm_final_model)
summary(vglm_final_model_nonparallel)

#Since the non_parallel p_values are most all greater than 0.05, we will stick with parallel one.
#Parallel = TRUE means that we’re assuming that the odds ratios are consistent across severity thresholds.

AIC(vglm_final_model)
#Now we will check how well the model fits to the data

#First, let's check on some of the residuals
#Calculate standardized residuals

pred_prob <- predict(vglm_final_model, type = "response")

#Define observed outcomes for each threshold
threshold_1 <- data_set$num3 < 1  # "None" vs "Mild + Severe"
threshold_2 <- data_set$num3 < 2  # "None + Mild" vs "Severe"

#Calculate response residuals for each threshold
residuals_threshold_1 <- threshold_1 - pred_prob[, 1]
residuals_threshold_2 <- threshold_2 - pred_prob[, 2]

#Standardized the residuals
std_residuals_threshold_1 <- residuals_threshold_1 / sqrt(pred_prob[, 1] * (1 - pred_prob[, 1]))
std_residuals_threshold_2 <- residuals_threshold_2 / sqrt(pred_prob[, 2] * (1 - pred_prob[, 2]))

#Plot for threshold 1
plot(std_residuals_threshold_1, 
     main = "Standardized Residuals for Threshold 1 (None vs Mild + Severe)", 
     ylab = "Standardized Residuals", 
     xlab = "Data Index", 
     pch = 16, col = "blue")
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 2)

#Plot for threshold 2
plot(std_residuals_threshold_2, 
     main = "Standardized Residuals for Threshold 2 (None + Mild vs Severe)", 
     ylab = "Standardized Residuals", 
     xlab = "Data Index", 
     pch = 16, col = "blue")
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 2)


#Try to find a similarity between the outlie data points but unable to find it.
outliers_SRT2 <- which(abs(std_residuals_threshold_2) > 2)
outlier_data_set <- data_set[outliers_SRT2, ]


#Let's check the ROC curve and the AUC

#ROC for first threshold
roc1 <- roc(threshold_1, pred_prob[, 1], plot = TRUE, col = "blue", 
            main = "ROC Curves for Both Thresholds", lwd = 2)
auc1 <- auc(roc1)

#ROC for second threshold
roc2 <- roc(threshold_2, pred_prob[, 2], plot = TRUE, col = "red", lwd = 2, add = TRUE)
auc2 <- auc(roc2)

#Add legend
legend("bottomright", legend = c(paste("Threshold 1 (AUC =", round(auc1, 2), ")"), 
                                 paste("Threshold 2 (AUC =", round(auc2, 2), ")")), 
       col = c("blue", "red"), lwd = 2)
