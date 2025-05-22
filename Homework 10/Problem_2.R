#Chad Huntebrinker
#Problem 5.1
library(readxl)

#a)
crabs <- read_excel("crab_data.xlsx")

model_1 <- glm(y ~ width + weight, family = binomial, data = crabs)
sum_model_1 <- summary(model_1)

model_reduced <- glm(y~1, family = binomial, data = crabs)

#Our null hypothesis is that B1 = B2 = 0 while our alternative hypothesis is that at least one
#predictor is significant to the model (either B1 != 0, B2 != 0 or both)
anova(model_reduced, model_1)

#The LR statistic is 32.9. Since our p-value < 0.05, we reject H0. That means that at least
#one of our predictors has a significant role on the model

#b)
model_no_weight <- glm(y ~ width, family = binomial, data = crabs)
model_no_width <- glm(y ~ weight, family = binomial, data = crabs)

anova(model_no_weight, model_1)
anova(model_no_width, model_1)

#The LR statistic is 1.56 for weight and 2.85 for width, with p-values of 0.21 and 0.09.
#One possible reason why neither test shows evidence of an effect while the previous step's test did is that
#when combined together, the variables provide valuable insight that can't be seen in a LR-test. So when they
#are separate, they seem useless to the model. But putting them together in the same model allows them to
#better explain what is going on in the model.

#c)
#First, build each model with one variable and compare their AICs
model_weight <- glm(y ~ weight, family = binomial, data = crabs)
model_spine <- glm(y ~ spine, family = binomial, data = crabs)
model_color <- glm(y ~ color, family = binomial, data = crabs)

summary(model_reduced)
summary(model_weight)
summary(model_spine)
summary(model_color)
#We will keep weight and color because their AICs are the best.

#Next, we will build a model with weight and color and compare them to their models with only weight or color
model_weight_color <- glm(y ~ weight + color, family = binomial, data = crabs)
summary(model_weight_color)
anova(model_weight, model_weight_color)
anova(model_color, model_weight_color)

#We find that the AIC for weight and color is better than when they are on their own and we
#see that there is increase in deviance when we remove color or weight from the model

#Next, add spine to our model
model_weight_color_spine <- glm(y ~ weight + color + spine, family = binomial, data = crabs)
summary(model_weight_color_spine)
anova(model_weight_color, model_weight_color_spine)
#The AIC isn't better by this addition and we find that the deviance doesn't increase by much. So, we
#remove spine from the model

#Finally, add an interaction between color and weight
model_weight_color_interaction <- glm(y ~ weight + color + weight*color, family = binomial, data = crabs)
summary(model_weight_color_interaction)
anova(model_weight_color, model_weight_color_interaction)
#We see the AIC doesn't improve and we also see the deviance has only a small increase.  As a result,
#we find our best model is with weight and color but no interaction.