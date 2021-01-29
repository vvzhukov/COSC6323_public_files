# 01/29/2021
# Vitalii Zhukov
# COSC 6323

# 
# MEAN
?mean()
# Create new vector:
x <- c(1,2,3,41,1,12,3,-23,12)
x.mean <- mean(x)

# Case with NA in observations:
x <- c(1,2,3,41,1,12,3,-23,12, NA, NA)
x.mean <- mean(x)
x.mean <- mean(x, na.rm = TRUE)

#
# MEDIAN
?median()
x.median <- median(x)

#
# MODE
?mode
?mod()
mode(x)
# different functionality here

# We can create custom function in order to show the value with
# the highest occurrence
?unique
?tablulate
?which.max()

math.mode <- function (x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
}

x <- c(1,2,3,41,1,12,3,-23,12,1,1)
math.mode(x)
# Lets check the result
?table
table(x)

# STANDARD DEVIATION
?sd()
sd(x)

# STANDARD ERROR
# SEM is calculated by taking the standard deviation and dividing 
# it by the square root of the sample size
stde <- function(y) sd(y)/sqrt(length(y))
stde(x)

# QUANTILE
?quantile
quantile(x)

# QUICK REVIEW
library(ggplot2)

?diamonds
str(diamonds)
summary(diamonds)

diamonds$price

mean(diamonds$price)
median(diamonds$price)
math.mode(diamonds$price)

# DISTRIBUTION GENERATORS
# https://rpubs.com/ggraham412/100906
# Probability Density - d...()
# Cumulative Distribution - p...()
# Quantile - q...()


## NORMAL
?dnorm()
# This function gives height of the probability distribution at each point 
# for a given mean and standard deviation.

# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(-10, 10, by = .1)
# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 1.5)
# Give the chart file a name.
plot(x,y)

?pnorm()
# Gives the probability of a normally distributed random number to be less that the 
# value of a given number. It is also called "Cumulative Distribution Function"

?qnorm()
# Takes the probability value and gives a number whose cumulative value matches 
# the probability value

?rnorm()
# generate random numbers whose distribution is normal. 
# It takes the sample size as input and generates that many random numbers.

## BINOMIAL
?dbinom()
# This function gives the probability density distribution at each point.

# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)
# Create the binomial distribution.
y <- dbinom(x,50,0.5)
# Plot the graph for this sample.
plot(x,y)

## POISSON
?dpois()

## EXPONENTIAL
?dexp()

## CHI-SQUARED
?dchisq()