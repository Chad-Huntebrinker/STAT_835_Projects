# Load necessary libraries
library(ggplot2)

# Create the dataset
shuttle_data <- data.frame(
  Ft = 1:23,
  Temp = c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 53, 67, 
           75, 70, 81, 76, 79, 75, 76, 58),
  TD = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 
         0, 0, 0, 0, 0, 1, 0, 1)
)

# Fit the logistic regression model
logit_model <- glm(TD ~ Temp, data = shuttle_data, family = binomial)

# Summary of the model
summary(logit_model)

# a) Interpretation of effect
coef(logit_model)  # Log-odds effect of temperature

# b) Estimate probability of distress at 31 degrees
test_data <- data.frame(Temp = 31)
prob_31 <- predict(logit_model, newdata = test_data, type = "response")
print(paste("Estimated probability of distress at 31°F:", prob_31))

# c) Find temperature where estimated probability is 0.5
logit_inv <- function(temp) {
  coef(logit_model)[1] + coef(logit_model)[2] * temp
}

temp_50 <- uniroot(function(temp) logit_inv(temp) - log(0.5 / (1 - 0.5)), 
                   lower = 0, upper = 100)$root
print(paste("Temperature at which estimated probability equals 0.5:", temp_50))

# Compute linear approximation for change in probability per degree at this temperature
prob_50 <- 1 / (1 + exp(-logit_inv(temp_50)))
slope_at_temp_50 <- coef(logit_model)[2] * (prob_50 * (1 - prob_50))
print(paste("Approximate change in probability per degree at", round(temp_50, 2), "F:", slope_at_temp_50))

# d) Interpret effect on odds
odds_multiplier <- exp(coef(logit_model)[2])  # Odds multiplier per degree increase
print(paste("Odds multiplier per degree increase in temperature:", odds_multiplier))

# e) Hypothesis test
anova_result <- anova(logit_model, test="Chisq")  # Test if temperature has a significant effect
print(anova_result)