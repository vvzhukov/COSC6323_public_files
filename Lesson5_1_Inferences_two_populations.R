# 02/26/2021
# Vitalii Zhukov
# COSC 6323

# Plan:
# 1. Inferences for two populations (2 tasks)
# 2. Project NA network review (data + gephi)
# 3. Review excersize

# TASK 1
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
# available between 4 cyl car and 8 cyl car.

# Alternative solution
# filter using multiple condition
mtcars_cyl_4_8 <- mtcars %>% filter(cyl %in% c(4,8))
# t test using two population from same dataset
t.test(mpg ~ cyl, data = mtcars_cyl_4_8)

# TASK 2
# Assuming that we have two groups of mice: before and 
# after treatment. 
# Is there any significant difference in the mean 
# weights after treatment?

# Data in two numeric vectors
# ++++++++++++++++++++++++++
# Weight of the mice before treatment
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

