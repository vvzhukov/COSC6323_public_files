# Created 02/04/2021
# Vitalii Zhukov
# COSC 6323
# Sources
# https://data-flair.training/blogs/hypothesis-testing-in-r/
# https://statistics.berkeley.edu/computing/r-t-tests

# Plan:
# 1. Theory
# 2. Student's t-Test
# 3. N number
# 4. Wilcoxon Rank Sum and Signed Rank Tests
# 5. Correlation, Variance and Covariance
# 6. Pearson's Chi-squared Test for Count Data
# 7. Exercise review
# 8. Gephi links (check BB)

# Null hypothesis 
# Alternate hypothesis

# A small p-value (typically ≤ 0.05) ?
# A large p-value (> 0.05) ?

# Hypothesis testing;

# Type I Error – Type I error occurs when the researcher rejects a null hypothesis when it is true. 
# The term significance level is used to express the probability of Type I error while testing the 
# hypothesis. The significance level is represented by the symbol α (alpha).

# Type II Error – Accepting a false null hypothesis H0 is referred to as the Type II error. 
# The term power of the test is used to express the probability of Type II error while testing 
# hypothesis. The power of the test is represented by the symbol β (beta).

# Student's T-test

# Student’s t-test or t-test (the real name is W.S. Gossett who hid his name due to his 
# position as a worker in a brewery company) is a simple yet very useful statistical test. 
# The basic idea behind t-test is the inference problem from a small sample size data set to 
# test whether its sample mean may have large deviation from the true population mean.
# Good explanation: https://suinotes.wordpress.com/2009/11/30/understanding-t-test-in-r/

# t.test(data.1, data.2) – The basic method of applying a t-test is to 
# compare two vectors of numeric data.

# var.equal = FALSE – If the var.equal instruction is set to TRUE, the variance 
# is considered to be equal and the standard test is carried out. If the instruction is set to FALSE (the default), the variance is considered unequal and the Welch two-sample test is carried out.

# mu = 0 – If a one-sample test is carried out, mu indicates the mean against 
# which the sample should be tested.

# alternative = “two.sided” – It sets the alternative hypothesis. The default value 
# for this is “two.sided” but a greater or lesser value can also be assigned. You can abbreviate the instruction.

# conf.level = 0.95 – It sets the confidence level of the interval (default = 0.95).

# paired = FALSE – If set to TRUE, a matched pair T-test is carried out.

# t.test(y ~ x, data, subset) – The required data can be specified as a formula of the 
# form response ~ predictor. In this case, the data should be named and a subset of the predictor variable can be specified.

# subset = predictor %in% c(“sample.1”, sample.2”) – If the data is in the form response ~ predictor, 
# the two samples to be selected from the predictor should be specified by the subset instruction from the column of the data.

# Example
?sleep
plot(extra ~ group, data = sleep)
t.test(extra ~ group, data=sleep)

# Answer (SPOILER)
#
#
#
# Based on the result, you can say: at 95% confidence 
# level, there is no significant difference 
# (p-value = 0.0794) of the two means. 
# Accept the null



# Two-Sample T-test with Unequal Variance
?set.seed()

set.seed(500)

?rnorm
x = rnorm(10)
y = rnorm(10)
test <- t.test(x,y)
test
# What does that mean?
# p < 0.05 => strong evidence against the null hypothesis, so you reject it

# Our study finds that Variable are on average 
round(test$estimate[1] - test$estimate[2], 3)
# higher in the X group compared to the Y group
# t-statistic, p and, CI

# the t-statistic is the ratio of the departure of the estimated value of a parameter 
# from its hypothesized value to its standard error. It is used in hypothesis testing 
# via Student's t-test. The t-statistic is used in a t-test to determine if you should 
# support or reject the null hypothesis
test$statistic
test$p.value
test$conf.int[1]
test$conf.int[2]

# The p-value is slightly different from the Welch version. For example:
t.test(x, y, var.equal = TRUE)

# One-sample T-test can be implemented as follows:
t.test(x, mu = 5)

# Using Directional Hypotheses in R
t.test(y, mu = 5, alternative = 'greater')


# Formula Syntax and Subsetting Samples in the T-test in R

# DATA
grass <- data.frame(
    rich = c(12, 15, 17, 11, 15, 8, 9, 7, 9),
    graze = c("mow", "mow", "mow", "mow", "mow", "unmow", "unmow", "unmow", "unmow") 
)

# You can create a formula by using the tilde (~) symbol.
t.test(rich ~ graze, data = grass)

# Subsetting in t.test:
t.test(rich ~ graze, data = grass, subset = graze %in% c("mow", "unmow"))


# SAMPLE SIZE
# Get n number

# Too small of a sample size can under detect the effect 
# of interest in your experiment

# Too large of a sample size may lead to unnecessary 
# wasting of resources
?pwr.t.test
# d - Effect size
# magnitude of the effect under the alternative hypothesis.
# The larger the effect size, the easier it is to detect an 
# effect and require fewer samples.

# sig.level - Significance level (Type I error)
# power - Power of test (1 - Type II error)
# type - type of t test (one-, two- or paired-samples)
pwr.t.test(d=0.3,power=0.75,
           sig.level=0.05,
           type="two.sample",
           alternative="greater")
# Numbers from exercise 2.10 p. 59 Cohen (1988)


# μ-test in R
# Wilcoxon Rank Sum and Signed Rank Tests
# When you have two samples to compare and your data is nonparametric, you can use the μ-test. 
# This goes by various names and may be known as the Mann—Whitney μ-test or Wilcoxon sign rank test. 
# The wilcox.test() command can carry out the analysis.


# test(sample.1, sample.2) – It carries out a basic two-sample μ-test on the numerical vectors 
# specified.

# mu = 0 – If a one-sample test is carried out, mu indicates the value against which the 
# sample should be tested.

# alternative = “two.sided” – It sets the alternative hypothesis. “two.sided” is the default value, 
# but a greater or lesser value can also be assigned. You can abbreviate the instruction but you still need the quotes.

# int = FALSE – It sets whether confidence intervals should be reported or not.

# level = 0.95 – It sets the confidence level of the interval (default = 0.95).

# correct = TRUE – By default, the continuity correction is applied. This can also be set to FALSE.

# paired = FALSE – If set to TRUE, a matched pair μ-test is carried out.

# exact = NULL – It sets whether an exact p-value should be computed. 
# The default is to do so for less than 50 items.

# test(y ~ x, data, subset) – The required data can be specified as a formula of the 
# form response ~ predictor. In this case, the data should be named and a subset of the 
# predictor variable can be specified.

# subset = predictor %in% c(″1″, ″sample.2″) – If the data is in the form response ~ predictor, 
# the subset instruction can specify the two samples to select from the predictor column of the data.

x = rnorm(10)
y = rnorm(10)
wilcox.test(x, y)

# One-Sample μ-test in R
wilcox.test(y, exact = FALSE)

# Correlation and covariance

# cor(x, y = NULL) – It carries out a basic correlation between x and y. 
# If x is a matrix or data frame, we can omit y. 
# One can correlate any object against any other object as long as the length of the individual 
# vectors matches up.

# cov(x, y = NULL) – It determines covariance between x and y. If x is a matrix or data frame, 
# one can omit y.

# cov2cor(V) – It takes a covariance matrix V and calculates the correlation.

# method = – The default is “pearson”, but “spearman” or “kendall” can be specified as the methods 
# for correlation or covariance. These can be abbreviated but you still need the quotes and note 
# that they are lowercase.

# var(x, y = NULL) – It determines the variance of x. If x is a matrix or data frame and y is 
# specified, it also determines the covariance.

# test(x, y) – It carries out a significance test of the correlation between x and y. 
# In this case, you can now specify only two data vectors, but you can use a formula syntax, 
# which makes it easier when the variables are within a data frame or matrix. 
# The Pearson product-moment is the default, but it can also use Spearman’s Rho or 
# Kendall’s Tau tests. You can use the subset command to select data on the basis of a grouping 
# variable.

# alternative = “two.sided” – The default is for a two-sided test but the alternative hypothesis 
# can be given as “two.sided”, “greater”, or “less”.

# level = 0.95 – If the method = “pearson” and n > 3, it will show the confidence intervals. 
# This instruction sets the confidence level and defaults to 0.95.

count = c(9,25,15,2,14,25,24,47)
speed = c(2,3,5,9,14,24,29,34)
cor(count, speed)
cor(count, speed, method = 'spearman')

# Covariance
?runif
?rnorm
x <- rnorm(30, sd=runif(30, 2, 50))

mat <- matrix(x,10)
?cov
V <- cov(mat)
V

# The cov2cor() command determines the correlation from a matrix of covariance, as shown 
# in the following command:
cov2cor(V)

data(women)
?women
cor.test(women$height, women$weight)

# attach() / with() syntax
data(cars)
?cor.test
cor.test(~ speed + dist, data = cars, method = 'spearman', exact = F)

