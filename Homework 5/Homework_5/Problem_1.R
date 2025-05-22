#Chad Huntebrinker
#Problem 1
#Part a
library("vcdExtra")

data_table <- matrix(c(2, 4, 13, 3,
                       2, 6, 22, 4,
                       0, 1, 15, 8,
                       0, 3, 13, 8), nrow = 4, byrow = TRUE)
rownames(data_table) <- c("<5", "5-15", "15-25", ">25")
colnames(data_table) <- c("Very Dissatisfied", "A Little Satisfied", "Moderately Satisfied", "Very Satisfied")

chi_results <- chisq.test(data_table)

chi_results
chi_results$stdres

#For deficiency, the chi-square test assumes that we have large samples to utilize it.
#However, if we do not, then the test can have unreliable results. A good way to check this is if any of
#the expected frequency are less than 5.  When we do this, we find a couple of examples (any
# of the rows with column 1) has an expected frequency less than 5.

#The standardized residuals suggest that as employees get paid more, their satisfaction grows as well.

#Part B
income_score <- c(3, 10, 20, 35)
sat_score <- c(1, 3, 4, 5)

cmh_results <- CMHtest(data_table, rscores = income_score, cscores = sat_score)
cmh_results$table[1]    #M^2
cmh_results$table[5]    #df
cmh_results$table[9]    #P-score

#The chi-square test does not take into account ordered categories, it just looks at if there is
#an association.  The M^2 test statistic does.
