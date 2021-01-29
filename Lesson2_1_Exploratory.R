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

# Case with trim
# Allows you to trim data after certain threshold
x <- c(1,2,3,41,1,12,3,-23,12)
x.mean <- mean(x, trim = 20)

#
# MEDIAN
?median()
x.median <- median(x)

#
# MODE
?mode
mode(x)
# different functionality here

# We can create custom function in order to show the value with
# the highest occurance

math.mode <- function (x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
}

x <- c(1,2,3,41,1,12,3,-23,12,1,1)
math.mode(x)
# Lets check the result
?table
table(x)

?quantile
quantile(x)

# 
library(ggplot2)

?diamonds
str(diamonds)
summary(diamonds)

diamonds$price

mean(diamonds$price)
median(diamonds$price)
math.mode(diamonds$price)
