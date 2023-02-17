# Created 02/26/2021
# Update 02/17/2023
# Vitalii Zhukov
# COSC 6323

# Refs:
# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
# https://rcompanion.org/rcompanion/d_09.html
# https://stat-methods.com/home/paired-samples-t-test-r/
# https://regenerativetoday.com/a-complete-guide-to-confidence-interval-t-test-and-z-test-in-r/

# Plan:
# 1. Inferences for two populations: Pooled t.test 
#   TASK 0 (detailed review)
#   TASK 1
#   EXERCISE    
# 2. Paired t-test 
#   TASK 2
#   TASK 3
#   WORKFLOW
#   EXERCISE

# 0. TASK 0
# Using the information in the heart disease data set, 
# find out if the Cholesterol level of the male population 
# is less than the cholesterol level of the female population 
# in the significance level of 0.05.

#install.packages("kmed)
library(kmed)
data(heart)
?heart

# Step 1. Set up the hypothesis and alpha level

# Lets start with the assumption that the mean cholesterol lvl
# for male and female population is the same. 

# H0: µ1 = µ2
# where µ1 - cholosterol lvl for male, µ2 - for female

# Based on the problem definition
# H1: µ1 < µ2
# and significance level α = 0.05

# Step 2. Select appropriate statistics

# We are going to use t-statistic to compare two means:
# Formula:
# t = (x̄1 - x̄2)/√((s1^2 / n1) + (s2^2 / n2))

# Degrees of freedom:
# df = ((s1^2 / n1) + (s2^2 / n2))^2 / 
#               ((s1^2 / n1)^2 / (n1-1)) + (s2^2 / n2)^2 / (n2-1))
# I agree, it looks scary :) will use R here...

# Step 3. Calculate statistic and p-value
t.test(heart$chol[heart$sex==T], heart$chol[heart$sex==F],
       alternative = 'less' , conf.level = 0.95)

# As per the output above, the t-statistic is -3.0135 and 
# the p-value is 0.001541.

# What if if will assume that the standard deviation 
# of the samples in this data are super close to the sd 
# in the general population?

# We will get almost a similar result with a z-test. 
# Library ‘BSDA’ has this function z.test. here is how to use it:
library(BSDA)
BSDA::z.test(heart$chol[heart$sex==T], 
             heart$chol[heart$sex==F], 
             alternative = "less", 
             mu = 0, 
             sigma.x = sd(heart$chol[heart$sex==T]), 
             sigma.y = sd(heart$chol[heart$sex==F]), 
             conf.level = 0.95)

# We got same statistic and similar p-value
# Parameters:
# Setting mu as 0 because our null hypothesis is the mean 
# cholesterol level of the male and female population is equal. 
# That means the difference between the two means is zero. 
# The mu parameter takes the difference in mean in the case 
# of two mean comparisons.

# Step 4. Come up with a decision rule
# p-value is less than the alpha we will reject the null hypothesis

# Step 5. Draw the conclusion
# p < 0.05 => H0 rejected
# we have enough evidence to reject the null hypothesis. 
# So, the cholesterol level in the male population is less 
# than the cholesterol level in the female population.

# 1. TASK 1
# Assuming that the data in mtcars follows the normal 
# distribution, determine if there is significant 
# difference between the mean gas mileage of 4 cyl 
# and 8 cyl cars.
library(dplyr)
data("mtcars")
mtcars4 <- mtcars %>% filter(cyl == 4)
mtcars8 <- mtcars %>% filter(cyl == 8)

t.test(mtcars4$mpg, mtcars8$mpg)
?t.test
# Results
# 1. In mtcars, the mean mileage of 4 cyl is 26.66364 mpg 
# and the 8 cyl is 15.1 mpg.

# 2. The 95% confidence interval of the difference in 
# mean gas mileage is between 8.318518 and 14.808755 mpg.

# 3. Here, p value is less than 0.05. So, we can reject 
# the null hypothesis. So, there is significant difference 
# between gas mileage for the 4 cyl car and 8 cyl car.

# Alternative solution
# filter using multiple condition
mtcars_cyl_4_8 <- mtcars %>% filter(cyl %in% c(4,8))
# t test using two population from same dataset
t.test(mpg ~ cyl, data = mtcars_cyl_4_8)


# 2. EXERCISE
# Determine if cars with the weight > 3000 lbs have more horse 
# powers comparing to the cars with the weight < 3000.
# Will solve it together at 5:50

?mtcars
t.test(subset(mtcars, wt > 3)$hp,
       subset(mtcars, wt < 3)$hp, 
       alternative = "greater")

# 3. TASK 2 (Paired, MEAN + check for normality)
# Assuming that we have two groups of mice: before and 
# after treatment. 
# Is there any significant difference in the mean 
# weights after treatment?

# Data in two numeric vectors
# ++++++++++++++++++++++++++
# Weight of the mice before treatment
# data("mice2", package = "datarium")
before <-c(200.1, 190.9, 192.7, 213, 241.4, 
           196.9, 172.2, 185.5, 205.2, 193.7)

# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 
          427.9, 422, 383.9, 392.3, 352.2)

# Create a data frame
my_data <- data.frame( 
    group = rep(c("before", "after"), each = 10),
    weight = c(before,  after)
)

# Summary statistics (using dplyr)
group_by(my_data, group) %>%
    summarise(
        count = n(),
        mean = mean(weight, na.rm = TRUE),
        sd = sd(weight, na.rm = TRUE)
    )


# Visualize
library(ggplot2)
ggplot(my_data, aes(x=group, y=weight)) +
    geom_boxplot() +
    ggtitle(paste("Mices: before and after experiment, n =",
                  nrow(my_data)))
# What is missing in the graph?

# Important questions before tests:
# Are two samples paired?
# Is the sample size large enough?
# How can we check normality?

# Use Shapiro-Wilk
# compute the difference
d <- with(my_data, 
          weight[group == "before"] - 
              weight[group == "after"])

# Shapiro-Wilk normality test for the differences
shapiro.test(d)
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: data not normally distributed

# RESULTS
# From the output, the p-value is greater than the 
# significance level 0.05 implying that the distribution 
# of the differences (d) are not significantly different 
# from normal distribution. In other words, we can assume 
# the normality.

# Option I. Compute t-test
res <- t.test(before, after, paired = TRUE)
res

# Option II. Compute paired t-test
res <- t.test(weight ~ group, data = my_data, paired = TRUE)
res

# RESULTS
# The p-value of the test is 6.210^{-9}, which is less 
# than the significance level alpha = 0.05. We can then 
# reject null hypothesis and conclude that the average 
# weight of the mice before treatment is significantly 
# different from the average weight after treatment 
# with a p-value = 6.210^{-9}.


# 4. TASK 3
# Paired t-test, Flicker feather example
# Yellowness index for Typical and Odd birds.
Input = ("
 Bird   Typical  Odd
 A     -0.255   -0.324
 B     -0.213   -0.185
 C     -0.190   -0.299
 D     -0.185   -0.144
 E     -0.045   -0.027
 F     -0.025   -0.039
 G     -0.015   -0.264
 H      0.003   -0.077
 I      0.015   -0.017
 J      0.020   -0.169
 K      0.023   -0.096
 L      0.040   -0.330
 M      0.040   -0.346
 N      0.050   -0.191
 O      0.055   -0.128
 P      0.058   -0.182  
")

Data = read.table(textConnection(Input),header=TRUE)

Difference = Data$Odd - Data$Typical

# Simple plot of difference
plot(Difference,
     pch = 16,
     ylab="Difference (Odd – Typical)")

abline(0,0, col="blue", lwd=2)

# A simple plot of differences between one sample 
# and the other.  Points below the blue line indicate 
# observations where Typical is greater than Odd, that 
# is where (Odd – Typical) is negative.

# Checking assumptions of the model
Data$Difference = Data$Odd - Data$Typical

hist(Difference,    
     col="gray",  
     main="Histogram of differences", 
     xlab="Difference")

shapiro.test(Difference)
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: data not normally distributed

t.test(Data$Typical, 
       Data$Odd, 
       paired=TRUE, 
       conf.level=0.95)

# 5. COMMON WORKFLOW:
#1. Produce descriptive statistics by group
#2. Perform the Shapiro-Wilk Test for Normality on each group
#3. Perform a hist/QQ plot of the differences
#4. Produce boxplots and visually check for outliers
#5. Perform an Independent Samples T-test


# 6. EXERCISE
# Is there is a significant difference in the mean 
# scores between a hw1 and hw2 scores? Is 2nd hw easier?
# Use alpha = 0.05
# Check for normality of the difference.
# https://github.com/vvzhukov/COSC6323_public_files/blob/main/lesson5.csv

# File lesson5.csv
# Try to solve on your own (5-7 minutes)




# load data
setwd("/Users/apple/Desktop/6323_TA/COSC6323_public_files-main")
exc6_data <- read.csv("lesson5.csv", header = T)

# create new vector with difference
exc6_diff <- subset(exc6_data, Assignment == "Homework #2")$Score - 
    subset(exc6_data, Assignment == "Homework #1")$Score

# library
library(ggplot2)
ggplot(data=exc6_data, aes(x=Assignment, y=Score)) + 
    geom_boxplot() +
    ggtitle(paste("Students score, n = ",nrow(exc6_data)/2))

# check for normality
shapiro.test(exc6_diff)

library("car")
qqPlot(exc6_diff)

# The t-test is not afraid of non-normal data. When there are more 
# than about 25 observations per group and no extreme outliers, 
# the t-test works well even for moderately skewed distributions 
# of the outcome variable.


t.test(subset(exc6_data, Assignment == "Homework #1")$Score,
       subset(exc6_data, Assignment == "Homework #2")$Score,
       alternative = "less",
       paired = T)

