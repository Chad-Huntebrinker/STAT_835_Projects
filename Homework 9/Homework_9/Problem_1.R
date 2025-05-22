#Chad Huntebrinker
#Problem 4.5
#Create the dataset
shuttle_data <- data.frame(
  Ft = 1:23,
  Temp = c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 53, 67, 
           75, 70, 81, 76, 79, 75, 76, 58),
  TD = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 
         0, 0, 0, 0, 0, 1, 0, 1)
)

logit_model <- glm(TD ~ Temp, data = shuttle_data, family = binomial)
summary(logit_model)

#a)
coef(logit_model)

#Temp coefficient is -0.2322. That means a one-degree increase in Temp decreases the logit by
#0.2322. Since p = 0.0320 < 0.05, Temp has a significant effect on our model.

#b)
new_data <- data.frame(Temp = 31)
prob_31 <- predict(logit_model, newdata = new_data, type = "response")
cat("Estimated probability of distress at 31 degrees F:", prob_31)

#c)
logit_function <- function(temp) {
  coef(logit_model)[1] + coef(logit_model)[2] * temp
}

temp_50 <- uniroot(function(temp) logit_function(temp) - log(0.5 / (1 - 0.5)), lower = 0, upper = 100)$root
print(paste("Temperature at 0.5:", temp_50))

#Compute linear approximation for change in probability per degree
#B * p * (1 - p)
slope_at_temp_50 <- coef(logit_model)[2] * (0.5 * (1 - 0.5))
print(paste("Approximate change in probability per degree at", temp_50, " degrees F:", slope_at_temp_50))

#d)
odds_multiplier <- exp(coef(logit_model)[2])
print(paste("Odds multiplier per degree increase in temperature:", odds_multiplier))
#For every 1 degree increase in temperature, the odds of thermal distress are multiplied by about 0.793.
#In other words, they are reduced by 79.28%

#e)
anova_result <- anova(logit_model, test="Chisq")  # Test if temperature has a significant effect
print(anova_result)
#p-value = 0.0048 (< 0.05), so we reject the null hypothesis. As a result, temperature significantly
#impacts the probability of thermal distress.