#Chad Huntebrinker
#Problem 8.1

#We need to apply the McNemar test to the following table and interpret. Our null hypothesis is that
#there is no association between smoking status and birth weight. Our alternative hypothesis is that
#smoking is associated with birth weight status, specifically low birth weights are more associated
#with a smoking status in the data.

smoker_data <- data.frame(
  NBW = c("nonsmoke", "nonsmoke", "smoke", "smoke"),
  LBW = c("nonsmoke", "smoke","smoke","nonsmoke"),
  count = c(159, 22, 14, 8)
)
View(smoker_data)

tab <- xtabs(count~NBW + LBW, data = smoker_data)

tab

mcnemar.test(tab, correct = FALSE)
#We see that the McNemar's chi-squared statistic is 6.53 and the p-value is about 0.011.
#Since that is < 0.05, we reject the null hypothesis and have strong evidence that low birth
#weight cases are more likely than normal birth weights to be smokers.