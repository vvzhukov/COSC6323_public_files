# Created: 02/10/2023
# Updated: -
# Vitalii Zhukov
# COSC 6323

# Based on:
# https://regenerativetoday.com/a-complete-guide-to-confidence-interval-t-test-and-z-test-in-r/
# https://www.r-bloggers.com/2015/07/hypothesis-testing-on-normally-distributed-data-in-r/
# https://blog.minitab.com/en/understanding-statistics/what-can-you-say-when-your-p-value-is-greater-than-005
# http://www.sthda.com/english/wiki/qq-plots-quantile-quantile-plots-r-base-graphs
# https://data.library.virginia.edu/understanding-q-q-plots/

# 1. What are the confidence interval and a basic manual calculation
# 2. z-test of one sample mean
# 3. t-test of one sample mean
# 4. QQ-plots
# 5. Test for one sample proportion and confidence interval

# ----------------------------------------
# 1. So what is CI (confidence interval)?
# ----------------------------------------

# Example 1. 
# Suppose a shopping mall wants to estimate the number of 
# customers it gets from 9 am to 12 pm on weekdays. 
# We are talking about the average number of customers the 
# mall has on weekdays between 9 am and 12 pm. How they will 
# approach this problem?
    
# 1. Take samples of about 100 weekdays, 
# and then calculate the mean. 

# Suppose the calculated mean is 42 people. 
# Assume the population standard deviation was 15.

# 2. From CLT, the sample mean should be close to the 
# true population mean. Here sample mean is the mean 
# that was calculated using the 100 samples above. 
# That may not be the true population mean. If 
# we take a sample of 1000 or 10000, this sample mean 
# may be different.

# How to infer the true population means from this sample mean?
    
# a range is inferred using the sample size, the sample mean, 
# and the population standard deviation, and it is assumed 
# that the true population means falls under this interval. 
# This interval is called a confidence interval.

# 3. In this example n ≥ 30 (where n is the number of data), 
# the sample mean is assumed to be normally distributed 
# with the population mean (which we do not know) and a 
# standard deviation of:

# σ/√n = 15/√100 = 1.5


# This standard deviation of 1.5 means that 95% of the 
# sample means will fall within 2 standard deviations of 
# the population mean (remember the 68–95–99 rule). 
# This also implies that 95% of the time the population 
# means will fall within 1.5 standard deviations of the 
# sample mean.

# General formula:
# Estimate ± margin of error

# here estimate - the sample mean 42

# margin of error tells you how far the original population 
# means might be from the sample mean and is calculated 
# using this formula:

# margin of error = Z*σ/√n
# Where z is the critical value. The value of z-critical 
# is fixed for every confidence level. 

# 75% z=1.15; 90% z=1.64; 95% z=1.96

# So the overall formula for the CI
# x̄ ± Z*σ/√n

# And for our example:
# 42+1.96*15/(√100)  42-1.96*15/(√100)
42+1.96*15/sqrt(100)
42-1.96*15/sqrt(100)
# 44.94 upper 39.06 lower

# Hypothesis testing
# A process of testing or finding evidence of any claim 
# concerning a population.

# ----------------------------------------
# 2. z-test of one sample mean
# ----------------------------------------

# Example 2
# New style of reading practice. Check if his new style is 
# helpful to the students to obtain a better score. It is 
# not possible to check if all the students in the world 
# do well with the new reading style. Took a sample of 60 
# students. The mean score went up to 6.5. The population 
# standard deviation is 11. Assume that we want to 
# determine if the new reading technique helped the 
# students improve their scores with a 95% confidence level.

# Lets first do manual process and then use R

# 1. State the H0 and H1
# In this example, we can state the null hypothesis as 
# there is no change in scores after using these new 
# reading techniques.
# H0: µ = 0
# If we do not find enough evidence for the null hypothesis, 
# we will reject the null hypothesis and say that the 
# alternative hypothesis is true. The alternative hypothesis is, 
# mean is greater than 0.
# H1: µ > 0

# 2. Select the appropriate test statistic. 
# Here sample size is 60. We know population SD.
# When we have relatively big (>30) sample size 
# and the population standard deviation is known
# we use z-statistic

# A test-statistic relating to the mean provides a 
# measure of how far the x-bar(sample mean) is from 
# the mu (population means) under the null hypothesis. 
# The formula for z-statistic is as follows:

# Z = (x̄ - µ) /  (σ/√n)
# Z follows a normal distribution with a mean of 0 
# and a standard deviation of 1.

# 3. Calculate Z-statistics
z_stat <- (6.5 - 0) / (11/sqrt(60))

# now we can calculate p-value
1-pnorm(z_stat)

# 4. Decision rule
# The confidence level was 95%, so the significance 
# level(alpha) is 5% or 0.05. We will reject the null 
# hypothesis if the alpha value is smaller than 
# the p-value. Otherwise, we will fail to reject the 
# null hypothesis.

# 5. Conclusion
# 2.35e-06 is very small, we have enough evidence to reject
# H0. New reading technique helps to improve reading score.

# Calculate in R
# a lot of ways here:
# custom function
# BSDA::z.test
# ASBIO::one.sample.z
# ...

install.packages("asbio")

library(asbio)
asbio::one.sample.z(null.mu = 0, xbar = 6.5, 
             sigma = 11, n = 60, 
             alternative = 'greater')

# ----------------------------------------
# 3. T-test on the sample mean
# ----------------------------------------

# Example
# A Scientist wanted to test if the great white 
# sharks are on average 20 feet in length. He 
# measured 10 great white sharks. The sample 
# mean is calculated to be 22.27 and the sample 
# standard deviation is 3.19. Did he find the 
# evidence that great white sharks are longer than 
# 20 feet in length at the α=0.05 level of significance?

# Same 5 steps procedure

# 1. Hypothesis and alpha level
# H0: µ = 20
# H1: µ > 20
# The alpha level is 0.05 (from the problem description).

# 2. Select appropriate statistics
# Sample size 10 (<30) and no info about
# population standard deviation.
# In this case we should use t-statistics

# t = (x̄ - µ)  / (s/√n)
# s - sample standard deviation

# 3. Calculate statistics
x = c(21.8, 22.7, 17.3, 26.1, 26.4, 
      21.1, 19.8, 24.1, 18.3, 25.1)

(mean(x) - 20) / (3.19/sqrt(10))

# R alternative
t.test(x, mu = 20, alternative = "greater")
# Here the t-statistic is 2.2523 and the 
# p-value is 0.02541. The degree of freedom is n-1. 
# Here n is 10(sample size). So the degree of freedom is 9.

# 4. State the decision rule
# If the p-value is less than or equal to alpha 
# (significance level) reject the null hypothesis. 
# Otherwise do not reject the null hypothesis.

# 5. Draw conclusion
# In this example, the p-value is 0.025 which is 
# less than the significance level alpha(0.05). So we 
# have enough evidence to reject the null hypothesis. 
# That means the mean length of great white sharks is 
# greater than 20.

# ----------------------------------------
# 4. QQ-plots
# ----------------------------------------

# What is a QQ plot?
# The Q-Q plot, or quantile-quantile plot, is a graphical 
# tool to help us assess if a set of data plausibly came 
# from some theoretical distribution such as a Normal or 
# exponential. For example, if we run a statistical analysis 
# that assumes our residuals are normally distributed, we 
# can use a Normal Q-Q plot to check that assumption. It’s 
# just a visual check, not an air-tight proof, so it is 
# somewhat subjective. But it allows us to see at-a-glance 
# if our assumption is plausible, and if not, how the 
# assumption is violated and what data points contribute to 
# the violation.

# A Q-Q plot is a scatterplot created by plotting two sets 
# of quantiles against one another. If both sets of quantiles 
# came from the same distribution, we should see the points 
# forming a line that’s roughly straight. Here’s an example 
# of a Normal Q-Q plot when both sets of quantiles truly 
# come from Normal distributions.

?ToothGrowth
# Store the data in the variable my_data
my_data <- ToothGrowth

# The R base functions qqnorm() and qqplot() 
# can be used to produce quantile-quantile plots:

# qqnorm(): produces a normal QQ plot of the variable
# qqline(): adds a reference line

qqnorm(my_data$len, pch = 1, frame = FALSE)
qqline(my_data$len, col = "steelblue", lwd = 2)

# Alternative 1
library("car")
qqPlot(my_data$len)

# Alternative 2 (advanced, recommended)
library(ggplot2)
p <- ggplot(my_data, aes(sample = len))
p + stat_qq() + stat_qq_line()


# ----------------------------------------
# 5. Test for one sample proportion and confidence interval
# ----------------------------------------

# The concept is not too different than the tests for means. 
# It tests how far the population proportion of a 
# larger population from a sample proportion. 

# Suppose we want to test the proportion of children who had 
# some swimming lessons when they were less than 10 years old. 
# We cannot go ask all the children in the world if they 
# had swimming lessons when they were less than 10. 
# So we will take a sample of 100, 1000, 5000, or the number 
# that is affordable to us and infer the information about 
# the large population from that sample.

# Conditions:
# The sample must be drawn randomly from the population. 
# Each observation in the sample must be independent of the 
# others. If sampling without replacement, the sample size 
# must be less than 10% of the population.

# Formula for z-statistics in prop test
# Z = (p̂-p0) / √(p0(1-p0)/n)

# p-hat is the claimed population proportion 
# p0 is the population proportion under the null hypothesis 
# n is the sample size.

# Confidence interval is:
# Population prop. or mean +- z-score * Standard Error

# Standard Error:
# SE = √(pop. proportion * (1-pop. proportion)/n)

# Example
# The local healthcare provider claimed that 50% of the 
# population of age 29 to 77 is suffering from some type 
# of heart disease. According to this Heart dataset, 
# approximately 46% of the population of age 29 to 77 
# have heart disease. We decided to calculate the 90% 
# confidence interval for the proportion of the 
# population of the specified age group suffering 
# from heart disease. Also, test if the population 
# proportion suffering from heart disease in this 
# specified age group is 50%. Total number of records in 
# the data set is 303. People with heart diseases - 139.


# Set hypothesis and alpha level
# From the problem confidence interval is 90%. 
# So the alpha level is 0.1.
# H0: p = 0.5
# H1: p != 0.5

# We will use prop.test function
# that will provide us with test-statistic, p-value, 
# and confidence interval everything

# Out of 303 people, 139 people have heart disease in the 
# dataset.
prop.test(139, 303, p=0.50, alternative = "two.sided",
          conf.level = 0.9)

# Look at the output carefully. 
# test-statistics chi-squared. 
# squared root of the chi-squared stats that is a z-statistic. 
# The p-value is 0.168 which is bigger than the alpha value. 
# We do not have enough evidence to reject the 
# null hypothesis. That means the population proportion 
# that suffers from heart disease within the age bracket of 
# 29 to 77 is 50% with a significance level of 0.1.

# Confidence interval is 0.41 to 0.51.
