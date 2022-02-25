# Created 02/26/2021
# Update 02/25/2022
# Vitalii Zhukov
# COSC 6323

# Refs:
# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
# https://rcompanion.org/rcompanion/d_09.html
# https://stat-methods.com/home/paired-samples-t-test-r/

# Plan:
# 1. Inferences for two populations: t.test 
#   (1 problem review, 1 exercise)
# 2. Paired t-test 
#   (2 problems review, workflow, 1 exercise)

# 1.1 TASK 1 (mean)
# Assuming that the data in mtcars follows the normal 
# distribution, determine if there is significant 
# difference between the mean gas mileage of 4 cyl 
# and 8 cyl cars.
library(dplyr)
data("mtcars")
mtcars4 <- mtcars %>% filter(cyl == 4)
mtcars8 <- mtcars %>% filter(cyl == 8)

t.test(mtcars4$mpg, mtcars8$mpg)

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


# EXERCISE
# Determine if cars with the weight > 3000 lbs have more horse 
# powers comparing to the cars with the weight < 3000.


# 2. TASK 2 (Paired, MEAN + check for normality)
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

ggplot(my_data, aes(x=group, y=weight)) +
    geom_boxplot() +
    ggtitle(paste("Mices: before and after experiment, n =",
                  nrow(my_data)))

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

# Common workflow:

#1. Produce descriptive statistics by group
#2. Perform the Shapiro-Wilk Test for Normality on each group
#3. Perform a hist/QQ plot of the differences
#4. Produce boxplots and visually check for outliers
#5. Perform an Independent Samples T-test

# Exercise
# Is there is a significant difference in the mean 
# scores between a pre-test and a post-test for 20 students
# Check for normality and psot the results.
read.csv("lesson5.csv")