#Chad Huntebrinker
#Problem 3.12

#An experiment analyzes imperfection rates for two processes used to fabricate silicon wafers
#for computer chips. For treatment A applied to 10 wafers, the numbers of imperfections are 8, 7,
#6, 6, 3, 4, 7, 2, 3, 4. The treatment B applied to 10 other wafers has 9, 9, 8, 14, 8, 13, 11, 5,
#7, 6 imperfections. Treat the counts as independent Poisson variates having means ua and ub.
#Consider the model logu =  a + Bx, where x = 1 for treatment B and x = 0 for treatment A, for which
#B = log ub - logua = log(ub/ua) and e^B = ub/ua.

#Part A
#Setup treatment and x-value vectors
treatment_A <- c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4)
treatment_B <- c(9, 9, 8, 14, 8, 13, 11, 5, 7, 6)

x_value_A <- rep(0, length(treatment_A))
x_value_B <- rep(1, length(treatment_B))
both_treatment <- c(treatment_A, treatment_B)
both_x <- c(x_value_A, x_value_B)

#Fit the model using both treatments and the x-values for both treatments
model_1 <- glm(both_treatment ~ both_x, family = poisson(link = "log"))
summary(model_1)

#Wald Test
B_hat <- coef(model_1)[2]
SE_B_hat <- summary(model_1)$coefficients[2, 2]

wald_stat <- B_hat / SE_B_hat
p_value <- 2 * (1 - pnorm(abs(wald_stat)))

cat("Wald Test statistic: ", wald_stat, "\n")
cat("Wald Test p-value: ", p_wald, "\n")

#Part B
#Get the z-score for 0.95 (we use 0.975 because it's a two-tailed test)
z_score <- qnorm(0.975, mean = 0, sd = 1)

#Confidence interval for B
lower_B <- B_hat - z_score * SE_B_hat
upper_B <- B_hat + z_score * SE_B_hat

#Exponentiate to get the confidence interval for the ratio u_B / u_A
lower_ratio <- exp(lower_B)
upper_ratio <- exp(upper_B)

#Print the confidence interval for u_B / u_A
cat("95% Confidence Interval for u_B / u_A: (", lower_ratio, ", ", upper_ratio, ")\n")