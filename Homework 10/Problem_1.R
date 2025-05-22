#Chad Huntebrinker
#Problem 4.19
library(ggplot2)
library(readxl)

#a)
crabs <- read_excel("crab_data.xlsx")
crabs$color <- factor(crabs$color, levels = c("4", "1", "2", "3"))


model_1 <- glm(y ~ width + color + width * color, family = binomial, data = crabs)
sum_model_1 <- summary(model_1)

#The equation is -5.85 + 0.20x + 4.10c1 - 4.19c2 - 15.66c3 - 0.09(x*c1) + 0.22(x*c2) + 0.66(x*c3)

#Next, we will be plotting and interpreting the prediction equation:
#First, need to create a data frame with our predictions by giving it
#a range of widths
width_seq <- seq(min(crabs$width), max(crabs$width), length.out = 100)
num_colors <- levels(crabs$color)

prediction_grid <- expand.grid(
  width = width_seq,
  color = num_colors
)

prediction_grid$probability <-  predict(model_1, newdata = prediction_grid, type = "response")

ggplot(prediction_grid, aes(x = width, y = probability, color = color)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Probability of Satelites",
    xlab = "Width",
    ylab = "Probability",
    color = "Crab Color"
  )
#What we see on the graph is that color 1 and even color 4 have a steady relation with width. As width
#increases, so does the probability that they will have a satelite. Color 2 and 3 have a sharp increase around
#22.5 width and it starts faltering off around 28 width.
  
#b)
model_2 <- glm(y ~ width + color, family = binomial, data = crabs)
sum_model_2 <- summary(model_2)

cat("AIC of interaction model: ", sum_model_1$aic, "\n")
cat("AIC of simplier model: ", sum_model_2$aic, "\n")

anova(model_2, model_1)
cat("Correlation for the simplier model: ", cor(crabs$y, fitted(model_2)), "\n")
cat("Correlation for the interaction model: ", cor(crabs$y, fitted(model_1)), "\n")
#The likelihood ratio statistic is 4.4 and the p-value is 0.22 (which is greater than 0.05). 
#The correlation for the simpler model is 0.452 and the correlation for the complex model is 0.472.
#As a result of these findings, we can say that simpler model fits almost as well as the interaction model
#and would be fine to use.