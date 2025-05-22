#Chad Huntebrinker
#Problem 2.17

#Part a
#Create the original matrix
race_gap <- matrix(c(871, 821, 336, 347, 42, 83), ncol = 3, byrow=TRUE)

#Assign row and col names
rownames(race_gap) <- c("White", "Black")
colnames(race_gap) <- c("Democrat", "Republican", "Independent")

#Calculate the sums for LRS
sum_of_rows <- rowSums(race_gap)
sum_of_cols <- colSums(race_gap)
total <- sum(race_gap)

#Get the expected frequency matrix
expected_frequency <- matrix(c(sum_of_rows[1] * sum_of_cols[1] / total, sum_of_rows[1] * sum_of_cols[2] / total,
                              sum_of_rows[1] * sum_of_cols[3] / total, sum_of_rows[2] * sum_of_cols[1] / total,
                              sum_of_rows[2] * sum_of_cols[2] / total, sum_of_rows[2] * sum_of_cols[3] / total),
                              ncol = 3, byrow=TRUE)

#Calculate the LRS
LRS <- 2 * sum(race_gap * log(race_gap / expected_frequency))
print(paste("Likelihood-Ratio Statistic: ", LRS))

#Get X^2 and p-value
chi_test <- chisq.test(race_gap)

chi_test
chi_test$p.value

#Because the p-value is 9.435104e-41, we can interpret that H0 is invalid. As result,
#we can say there is no independence and there is an association between race and political party.

#Part b
#Get the standardized residuals
chi_test$stdres
#The standardized residuals for White Democrats and Black Republicans show strong evidence of fewer
#people in these cells than if they were independent of race.  And the standardized residuals of White
#Republican and Black Democrat show strong evidence of more people in these cells than if they were
#independent of race.

#Part c
matrix_1 <- matrix(c(871, 821, 347, 42), ncol = 2, byrow=TRUE)

sum_of_rows <- rowSums(matrix_1)
sum_of_cols <- colSums(matrix_1)
total <- sum(matrix_1)

#Calculate the expected frequency matrix
expected_frequency <- matrix(c(sum_of_rows[1] * sum_of_cols[1] / total, sum_of_rows[1] * sum_of_cols[2] / total,
                               sum_of_rows[2] * sum_of_cols[1] / total, sum_of_rows[2] * sum_of_cols[2] / total),
                             ncol = 2, byrow=TRUE)

#Calculate the LRS
LRS2 <- 2 * sum(matrix_1 * log(matrix_1 / expected_frequency))
print(paste("Likelihood-Ratio Statistic: ", LRS2))

LRS - LRS2

pchisq(LRS2, 1, lower.tail = FALSE)
pchisq(LRS - LRS2, 1, lower.tail = FALSE)
#LRS2 is for comparing races on (Democrat, Republican) choice and the above score of about 0.3
#is for comparing races on (Democrat + Republican, Independent). According to their p-score, there is
#strong evidence that of a difference in the relative numbers between races and
#if they identify as Democrat or Republican but no strong evidence between races in the relative
#numbers identifying as Independent instead of Democrat or Republican.