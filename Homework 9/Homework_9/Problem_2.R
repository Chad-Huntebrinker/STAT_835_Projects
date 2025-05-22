#Chad Huntebrinker
#Problem 4.9
library(arm)
library(readxl)
#Get the dataset
crabs <- read_excel("crab_data.xlsx")

#a)
model_1 <- glm(y ~ factor(crabs$color, levels = c("4", "1", "2", "3")), family = binomial, data = crabs)
summary(model_1)

#The prediction equation is: logit(π)= −0.76 + 1.86c1 + 1.74c2 + 1.13c3

#c1 = 1 if the crab is color 1
#So it would be -0.7621 + 1.8608 = 1.10
#When it's color 4, all the other covariates are 0 except for the intercept. So it would be -0.76
#Since the difference between the two is about 1.86 we need to find
exp(1.86)
#Thus, the estimated odds a medium-light crab has a satelite is 6.4 times the odds of a dark crab.

#b)
model_null <- glm(y ~ 1, family = binomial, data = crabs)
anova(model_null, model_1, test = "Chisq")

#c)
model_2 <- glm(y ~ as.numeric(color), family = binomial, data = crabs)
summary(model_2)
anova(model_null, model_2, test = "Chisq")

#d)
#Advantage (Power): Treating color as quantitative reduces the number of parameters, increasing power 
#for detecting an effect.

#Disadvantage (Lack of Fit): Treating color as quantitative could lead to making assumptions (like 
#assuming the color is linear when it isn't) which could lead to a  model lack of fit.

#e)
model_full <- glm(y ~ weight + as.numeric(color), family = binomial, data = crabs)

# Standardize coefficients
model_standardized <- standardize(model_full)
summary(model_standardized)