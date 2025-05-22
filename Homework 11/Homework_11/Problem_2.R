#Chad Huntebrinker
#Problem 6.8
#We need to fit a cumulative logit model  with a proportional odds structure, interpret the estimated
#treatment effect, and check whether a model allowing interaction provides a significant better fit.

library(VGAM)
library(readxl)

#Create the dataframe
data <- data.frame(
  Treatment = factor(rep(c("Sequential", "Alternating"), each = 6)),
  Gender = factor(rep(rep(c("Male", "Female"), each = 3), 2)),
  Response = ordered(rep(c("NoChange", "Partial", "Complete"), 4),
                     levels = c("NoChange", "Partial", "Complete")),
  Count = c(45, 29, 26,
            12, 5, 2,
            44, 20, 20,
            7, 3, 1)
)

#Fit cumulative logit model
model_1 <- vglm(Response ~ Treatment + Gender,
                   family = cumulative(parallel = TRUE, link = "logitlink"),
                   weights = Count,
                   data = data)


summary(model_1)
#The logit model was fit to evaluate the effects of treatment type and gender on chemotherapy response.
#The estimated treatment effect comparing Sequential to Alternating therapy found the odds ratio
#as = 0.81 (exp(-0.2152)) with a p-value of 0.41. The estimated treatment effect comparing
#the two genders had an odds ratio of 0.49 (exp(-0.7149)) with a p-value of 0.07. This showed that
#Sequential treatment had about 19% lower odds of having a better remission (complete remission
#being the best) and 51% lower odds of females having a better remission. However, since both of these
#estimations had a p-value < 0.05, neither one of them are significant to the response.

#Next, we'll see if a model with interaction fits better.
model_2 <- vglm(Response ~ Treatment + Gender + Treatment:Gender,
                family = cumulative(parallel = TRUE, link = "logitlink"),
                weights = Count,
                data = data)

summary(model_2)

#We'll do a Likelihood ratio test to see if the interaction model gives us a better fit.
#Our null hypothesis is that the model with interaction does not provide a better fit, while our
#alternative hypothesis is that the model with the interaction does.
lrtest(model_1, model_2)
#With a p-value of 0.8, we can't reject our null hypothesis. So our model with an interaction term does not
#give us a better fit.