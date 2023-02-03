# Created 02/03/2023
# Vitalii Zhukov
# COSC 6323
# Sources
# https://www.r-bloggers.com/2021/06/how-to-find-z-score-in-r-easy-calculation-quick-guide/
# https://bookdown.org/mrenna/statbook/statistical-inference.html#hypothesis-testing

# Plan:
# 1. Sampling distribution
# 2. Sampling size and Sampling distribution
# 3. Z score


# SAMPLING DISTRIBUTION
library(dplyr)
ames <- read.csv("http://bit.ly/315N5R5") 
dplyr::glimpse(ames) # you need dplyr to use this function

area <- ames$Gr.Liv.Area
price <- ames$SalePrice

head(area, n=10) # show first 10 observations 
head(price, n=10) # show first 10 observations 

length(area) # how many observations in the vector?

any(is.na(area)) #is there any NA in the vector area?
area.pop.sd <- sqrt(sum((area - mean(area))^2)/(2930)) # Population standard deviation
area.pop.sd

# alternatively
sd(area)

summary(area)

hist(area,
     main = "Histogram of above ground living area",
     xlab = "Above ground living area (sq.ft.)",
)


area <- ames$Gr.Liv.Area # create new dataset containing only variable 'Gr.Liv.Area' from dataset 'ames'
samp1 <- sample(area, 50) # take a random sample of 50 observations from the dataset 'area'
mean(samp1) # mean of the sample distribution for area. Note difference from population mean.


# Exercise (sampling, comparing means)
# 1. make two samples n1 = 50, n2 = 1000
# compare means

# 2. suppose we have samples n3 = 1500, n4 = 2000. 
# which is more accurate?



area <- ames$Gr.Liv.Area 
sample_means50 <- rep(NA, 500) # initialise a vector

for (i in 1:500) { # use of a loop function to draw a random sample 5000 times
    samp <- sample(area, 50)
    sample_means50[i] <- mean(samp)
}

hist(sample_means50, breaks = 25, 
     main = "Sample mean for Above ground living area",
     xlab = "Means (sq.ft.)") #Histogram of the 500 samples (sampling distribution of the samples mean)

# What happens to the distribution if we instead 
# collected 5,000 sample means?

# How will the shape of the distribution change?

# Exercise 
# To make sure you understand what you’ve done in this 
# loop, try running a smaller version. Go to RStudio and 
# initialise a vector of 100 zeros called 
# sample_means_small. Run a loop that takes a sample 
# of size 50 from area and stores the sample mean in 
# sample_means_small, but only iterate from 1 to 100. 

# How many elements are there in this object 
# called sample_means_small? What does each element 
# represent?


# SAMPLING SIZE AND DISTRIBUTION
# To get a sense of the effect that sample size has on 
# our distribution, let’s build up two more sampling 
# distributions: one based on a sample size of 10 and 
# another based on a sample size of 100 from a 
# population size of 5000.

area <- ames$Gr.Liv.Area 
sample_means10 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)

for(i in 1:5000){
    samp <- sample(area, 10)
    sample_means10[i] <- mean(samp)
    samp <- sample(area, 100)
    sample_means100[i] <- mean(samp)
}

# Lets plot data to see the differences


area <- ames$Gr.Liv.Area 
sample_means10 <- rep(NA, 5000)
sample_means50 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)

for(i in 1:5000){
    samp <- sample(area, 10)
    sample_means10[i] <- mean(samp)
    samp <- sample(area, 50)
    sample_means50[i] <- mean(samp)
    samp <- sample(area, 100)
    sample_means100[i] <- mean(samp)
}

par(mfrow = c(3, 1))  # this creates 3 rows and 1 column for graphs

xlimits <- range(sample_means10)

hist(sample_means10,  breaks = 25, xlim = xlimits)
hist(sample_means50,  breaks = 25, xlim = xlimits)
hist(sample_means100, breaks = 25, xlim = xlimits)

# Try different sample sizes.
# When the sample size is larger, what happens to the center? 
# What about the spread?
# A. The center moves to the right; standard deviation gets smaller

# B. The center approaches to the true mean; 
# the standard deviation does not change

# C. The center approaches to the true mean; 
# the standard deviation gets smaller

# Exercise
# 1. Take random sample of size 50 from price
# 2. Plot the data, then describe the shape of this sampling 
#   distribution. Based on this sampling distribution, what 
#   would you guess the mean home price of the population to be?
# 3. Calculate and report the population mean.
# 4. Change sample size to 1500. Describe the shape.
# 5. From two distributions which has a smaller spread? Why?

# HYPOTHESIS TESTING
# Z-score

# How to find? 
# Z-score provides how many standard deviations away a value 
# is from the mean.

# We can use the following formula for z-score calculation.
# Z-score = (x-μ)/σ
# x is a raw score to be standardized;
# μ is the mean of the population;
# σ is the standard deviation of the population.

# Example 1, single vector
data <- c(8, 7, 7, 10, 13, 14, 15, 16, 18) 
z_scores <- (data-mean(data))/sd(data)
z_scores

# The first raw data value of “8” is 0.9701425 standard deviations below the mean.
# The second raw data value of “7” is -1.2126781 standard deviations below the mean.
# The ninth raw data value of “18” is 0.2425356 standard deviations above the mean.

dev.off()
plot(z_scores, type="o", col="red")

# what if we will calculate z-scores for the standard normal distribution?
data_norm <- rnorm(100, mean = 0, sd = 1)
z_scores_norm <- (data_norm-mean(data_norm))/sd(data_norm)
plot(z_scores_norm, type="o", col="red")

# how can we get a better picture?
plot(sort(z_scores_norm), type="o", col="red")
plot(x = data_norm, y = z_scores_norm)

# Example 2, Single Column in a DataFrame
data<- data.frame(A = c(2, 5, 6, 7, 9, 13),
                  B = c(24, 20, 13, 15, 19, 20),
                  C = c(15, 5, 7, 18, 14, 10))
z_scores <- (data$B-mean(data$B))/sd(data$B)
z_scores

# The first raw data value of “24” is 1.3970014 standard deviations 
# above the mean.
# The first raw data value of “20” is 0.3810004  standard deviations 
# above the mean.

# Example 3, Every Column in a DataFrame
sapply(data, function(data) (data-mean(data))/sd(data))

# Application example
# One of the common examples is a student’s class marks for an 
# exam that appears to be normal and it has a mean of 50. 
# Now we want to know one of the best students who scored a 75 
# is among the top 10% of the scorers or not. 

# z score test statistic makes this calculation fairly easy.
# Z = (value – mean)/ (Standard Deviation)

# Using a z table, you can get the corresponding p-value test 
# statistic for this z score, it indicates whether a 
# score of 75 is in the top 10% of the class or not.

# In general, the z score tells you how far a value is 
# from the average of the data in terms of standard deviations.

# qnorm in r - function to calculate z score in r
qnorm(0.75, mean = 0, sd = 1)
# Its value being below 1 means that the point that separates the 
# lower 75% observations and upper 25% observations is within one 
# standard deviation of the average, towards the right.


# HYPOTHESIS TESTING


# Dataset from North Carolina, USA. In 2004, the state of North 
# Carolina released a large data set containing information on 
# births recorded in this state

nc <- read.csv("http://bit.ly/31adfCe")
glimpse(nc) # just glimpse of how the data looks like
summary(nc)

library(ggplot2)

ggplot(data = na.omit(nc), 
       aes(x = habit, y = weight, colour = habit))  +   
    geom_boxplot() + xlab("Smoking Habit")      +   
    ylab("Baby's Weight")                       +        
    ggtitle("Mother's Habit vs baby's Weight")  +  
    stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 1, size = 2)

# same with the base plotting function
data = na.omit(nc)
boxplot(data$weight ~ data$habit ,
        main = "Mother's Habit vs baby's Weight",
        ylab = "Baby's Weight",
        xlab = "Smoking Habit",
        col = c("red", "green")
)
legend("topleft", c("nonsmoker", "smoker"), 
       fill = c("red", "green"))

# Before doing any testing check if conditions
# that are required for the test are satisfied
# Check: outliers, group sizes 

# Group sizes
by(nc$weight, nc$habit, length)

library(statsr)
statsr::inference(y = weight, x = habit, data = nc, 
                  statistic = c("mean"), 
                  type = c("ci"), 
                  null = 0,
                  alternative = c("twosided"), 
                  method = c("theoretical"), 
                  conf_level = 0.95,
                  order = c("smoker","nonsmoker"))

# carefully review the arguments

data = na.omit(nc) # let's omit missing values from our calculations

inference(y = weight, x = habit, data = data, 
          statistic = c("mean"), 
          type = c("ci"), 
          null = 0,
          alternative = c("twosided"), 
          method = c("theoretical"), 
          conf_level = 0.95,
          order = c("smoker","nonsmoker"))

# As the confidence interval does not include the null value (i.e., 0),
# we can say that the difference we observe is statistically 
# significant and different from zero

# Lets double check it using t-test
inference(y = weight, x = habit, data = data, 
          statistic = c("mean"), 
          type = c("ht"), 
          null = 0,
          alternative = c("twosided"), 
          method = c("theoretical"), 
          conf_level = 0.95,
          order = c("smoker","nonsmoker"))