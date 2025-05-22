#Chad Huntebrinker
#Problem 6.2
library(readxl)
library(VGAM)

#We need to use a model fit to estimate an odds ratio that describes the effect of length on primary food
#choice being either invertebrate or other.

alligator_data <- read_excel("alligator_data.xlsx")

#We're subsetting the data to only keep I and O.
subset_data <- subset(alligator_data, y %in% c("I", "O"))

#Create the model
model <- vglm(y ~ x, data = subset_data, family = cumulative(parallel = TRUE, link = "logitlink"))
summary(model)

#Now we get the odds ratio
cat("Odds ratio:", exp(coef(model)["x"]), "\n")

#For each one-unit increase in length, the odds that the alligator chooses Invertebrate (vs Other) as
#its primary food decrease by about 89% (since 1 - 0.113 = 0.887).