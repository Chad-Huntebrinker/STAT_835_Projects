#Chad Huntebrinker
#Problem 7.3

#Part a
#We have table that record automobile accidents and whether the person had a seatbelt on or not
#We need to find a loglinear model that describes the data well and interpret the associations.

motor_data <- data.frame(
  S = c("used", "used", "used", "used", "not used", "not used", "not used", "not used"),
  E = c("yes", "yes", "no", "no", "yes", "yes", "no", "no"),
  I = c("nonfatal", "fatal", "nonfatal", "fatal", "nonfatal", "fatal", "nonfatal", "fatal"),
  count = c(1105, 14, 411111, 483, 4624, 497, 157342, 1008)
)

model_1 <- glm(count ~ S*E*I, family = poisson, data = motor_data)
summary(model_1)

#We see the p-value for the full model's 3-way interaction term is 0.113. Since it's > 0.05, we should
#try removing it from the model and see what we get.

model_2 <- glm(count ~ S+E+I+S:E+S:I+E:I, family = poisson, data = motor_data)
summary(model_2)

#What we find is AIC is just as good for model_2 as it is model_1 and the deviance for model_2 is small
#too (it's 2.854). And since model_2 is a simpler model, we'll stick with that one.
#We see that the log-odds for S:E is e^(-2.399) which is about 0.091, SI is e^(1.717) = 5.567,
#and E:I is e^(-2.797) = 0.061. That means that seatbelts reduce ejection odds by 91%, seatbelts increase the
#odds of surviving an accident by 5.567 times, and ejection reduces the chances of surviving an accident by
#94%.