n <- 10  # Number of trials
y <- 8   # Number of successes
p0 <- 0.5  # Hypothesized probability under the null hypothesis

# P-value for Ha: pi > 0.5 (Right-tailed test)
p_value_right <- dbinom(8, n, p0) + dbinom(9, n, p0) + dbinom(10, n, p0)

# P-value for Ha: pi < 0.5 (Left-tailed test)
p_value_left <- pbinom(y, n, p0)

# Mid-p-value for Ha: pi > 0.5 (Right-tailed test)
p_Y_equals_8 <- dbinom(8, n, p0)  # Probability of X = 8
p_Y_equals_9 <- dbinom(9, n, p0)  # Probability of X = 9
p_Y_equals_10 <- dbinom(10, n, p0) # Probability of X = 10
mid_p_value_right <- p_Y_equals_9 + p_Y_equals_10 + 0.5 * p_Y_equals_8

# Mid-p-value for Ha: pi < 0.5 (Left-tailed test)
mid_p_value_left <- (pbinom(y - 1, n, p0)) + 0.5 * p_Y_equals_8

# Output results
cat("Right-tailed p-value (Ha: pi > 0.5):", p_value_right, "\n")
cat("Left-tailed p-value (Ha: pi < 0.5):", p_value_left, "\n")
cat("Mid-p-value (Right-tailed):", mid_p_value_right, "\n")
cat("Mid-p-value (Left-tailed):", mid_p_value_left, "\n")