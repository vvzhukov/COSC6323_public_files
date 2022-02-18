# Created: 02/11/2021
# Updated: 02/18/2022
# Vitalii Zhukov
# COSC 6323

# Based on:
# https://www.r-bloggers.com/2015/07/hypothesis-testing-on-normally-distributed-data-in-r/

# Plan:
# 
# 1. HYPOTHESIS TEST ON Mean (custom function, example, exercise)
# 2. HYPOTHESIS TEST ON Variance (example, exercise)
# 3. HYPOTHESIS TEST ON Proportion (example, exercise)
# 4. HW discussion
# 5. DPLYR data manipulation (if we still have time)
# 6. KS fitting (if we have time)


# HYPOTHESIS TEST ON MEAN
# Perform a z-test under the assumptions that the 
# data is normally distributed
# 
# Remember that:
# 1.Low pvalue: strong empirical evidence against h0
# 2.High pvalue: little or 'no' empirical evidence against h0

# Should Texas DMV invest money in the highway safety driving research
# project based on the speed observed in the left lane of the highway.
# Critical speed when the research will be viable is 73 mph.
# Is the mean of the observed data different from the critical region?
# Greater? Smaller?

data_vector <- c(63, 75, 84, 58, 52, 
                 96, 63, 55, 76, 83, 
                 62, 61, 60, 62, 64, 
                 81, 80, 82, 65, 63,
                 62, 61, 71, 59, 50,
                 74, 59, 57, 68, 81)

# Left tail test
# H0: mu >= mu0
# H1: mu < mu0
t.test.left <- function(data, mu0, alpha)
{
    t.stat <- (mean(data) - mu0) / (sqrt(var(data) / length(data)))
    dof <- length(data) - 1
    t.critical <- qt(alpha, df= dof)
    p.value <- pt(t.stat, df= dof)
    if(t.stat <= t.critical)
    {
        print("Reject H0")
    }
    else
    {
        print("Accept H0")
    }
    print(paste('T statistic:', round(t.stat,5)))
    print(paste('T critical value:',round(t.critical,5)))
    print(paste('P value:', round(p.value,5)))
    return(t.stat)
}

t.test.left(data_vector, 73, 0.05)
# And the solution from R:
t.test(data_vector, mu = 73, alternative = "less")

# Right tail test
# H0: mu <= mu0
# H1: mu > mu0
t.test.right <- function(data, mu0, alpha)
{
    t.stat <- (mean(data) - mu0) / (sqrt(var(data) / length(data)))
    dof <- length(data) - 1
    t.critical <- qt(1-alpha, df= dof)
    p.value <- 1 - pt(t.stat, df= dof)
    if(t.stat >= t.critical)
    {
        print("Reject H0")
    }
    else
    {
        print("Accept H0")
    }
    print(paste('T statistic:', round(t.stat,5)))
    print(paste('T critical value:',round(t.critical,5)))
    print(paste('P value:', round(p.value,5)))
    return(t.stat)
}

t.test.right(data_vector, 73, 0.05)
# And the solution from R:
t.test(data_vector, mu = 73, alternative = "greater")

# Two tail z test
# H0: mu = mu0
# H1: mu != mu0
t.test.twoTails <- function(data, mu0, alpha)
{
    t.stat <- abs((mean(data) - mu0)) / (sqrt(var(data) / length(data)))
    dof <- length(data) - 1
    t.critical <- qt(1-alpha/2, df= dof)
    p.value <- 2*(1-pt(t.stat, df= dof))
    if(t.stat >= t.critical)
    {
        print("Reject H0")
    }
    else
    {
        print("Accept H0")
    }
    print(paste('T statistic:', round(t.stat,5)))
    print(paste('T critical value:',round(t.critical,5)))
    print(paste('P value:', round(p.value,5)))
    return(t.stat)
}
t.test.twoTails(data_vector, 73, 0.05)
# And the solution from R:
t.test(data_vector, mu = 73, alternative = "two.sided")


# Exercise (7 minutes)
# In the mtcars data is there a significant evidence that cars with 
# 3 forward gears has less than 18 miles per gallon fuel consumption? 
data(mtcars)


# HYPOTHESIS TEST ON Variance (SIGMA^2)

# We have a pump at the gas station which seems to 
# fill not consistent amount of fuel.
# To monitor the accuracy of the pump, a sample of 10 
# canisters was taken. Each canister volume is 14 gallons.

# Technical requirement is not only that the pump will 
# fill canisters with 14 gallons, but also maintain 
# a consistent amount of fuel being put into the canister.

# The variance should be 0.01 and the measured variance
# is SS = 0.3211

# We want to test the hypotheses
# H0: sigma^2 = 0.01 vs sigma^2 > 0.01

# The rejection is based on the statistic
# X^2 = SS / 0.01

# X^2 = 32.11
# Now we compare it with a Chi-Squared distribution with 
# 9 degrees of freedom:

# Critical region value is:
qchisq(0.95,9)


# Since the computed statistic value 32.11 is greater 
# than the critical value 16.91, the null hypothesis 
# is rejected.

# The pump should be replaced.

# Another example
library(EnvStats)
observed_data <- c(12.43, 11.71, 14.41, 11.05, 9.53, 
                   11.66, 9.33, 11.71, 14.35, 13.81,
                   10.16, 9.01, 13.77, 14.00, 11.31) 

varTest(observed_data, alternative="greater", conf.level = 0.95,
        sigma.squared = 2.25)

# Exercise (7 minutes)
# For the mtcars data is the variance of the 8 cylinder cars weight 
# more than 300lbs?
data(mtcars)

# HYPOTHESIS TEST ON Proportions

# Winter has come to Westeros. Winterfell needs new warriors.
# One of the Ravens told the King that newly born babies are more 
# likely to become warriors than any other profession so he doesn't 
# need to change his offensive strategy.
# However King is wise and he assigned Hand of the King to review the 
# problem from the statistical point of view.

# A random sample found 13,173 warriors (soldiers) were born among 25,468 
# newborn children. The sample proportion of soldiers 
# was 0.5172. Is this sample evidence that the birth of 
# soldier is more common than the birth of non-soldier in the 
# entire population? What should Lord hand include in the report for the King?

# The hypotheses are
# H0 :p≤0.5
# H1 :p>0.5
# Note that this is a one-tailed test.

# The test statistic is
p_hat = 13173/25468
p0 = 0.5
n = 25468
z = (p_hat - p0)/sqrt(p0*(1-p0)/n)

# The p value is
pnorm(z, lower.tail = FALSE)

# Since the p value is less than 0.05, we can reject H0.

# We would say that there is sufficient evidence to 
# conclude soldiers are more common than other professions in the 
# entire population. Raven was right!

# Alternative solution:
prop.test(13173, 25468, p=0.5, alt="greater",
          correct=FALSE)

# Exercise (7 minutes)
# In the StarWars movies are there more male or female characters?
# Is the proportion greater than 0.15?
library(dplyr)
?starwars
