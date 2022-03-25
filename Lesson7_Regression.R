# Created: 03/11/2021
# Modified: 03/11/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. https://www.tutorialspoint.com/r/r_linear_regression.htm
# 2. http://www.sthda.com/english/articles/39-regression-model
# 3. Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.
# 4. James, Gareth, Daniela Witten, Trevor Hastie, and Robert Tibshirani. 2014. An Introduction to Statistical Learning: With Applications in R. Springer Publishing Company, Incorporated.


# PLAN:
# EXAMPLE 1 (lm(), predict(), plot)
# EXAMPLE 2 (train vs test, diagnostics) [3 BONUS Points]
# EXTRA PROBLEM 1 (if we have time)

# EXAMPLE 1

# PREDICTOR (height)
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# RESPONCE (weight)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
?lm()
relation <- lm(y~x)

print(relation)

print(summary(relation))


# PREDICT FUNCTION

# Apply the lm() function.
relation <- lm(y~x)

# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)


# VISUALIZATION

# Plot the chart.
plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)), cex = 1.3, pch = 16,
     xlab = "Weight in Kg",
     ylab = "Height in cm")


# EXAMPLE 2

library(tidyverse)
library(caret)
library(broom)

# Load the data
# predicting sales units on the basis of the amount of money 
# spent in the three advertising medias
data("marketing", package = "datarium")
??marketing
# Inspect the data
sample_n(marketing, 3)

# OPTIONAL
# Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales %>%
    createDataPartition(p = 0.8, list = FALSE)

train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]


# Build the model
model <- lm(sales ~ youtube, data = marketing)
summary(model)$coef

# Make predictions
newdata <- data.frame(youtube = c(0,  1000))
model %>% predict(newdata)

?augment
model.diag.metrics <- augment(model)
head(model.diag.metrics)

# Among the table columns, there are:
# youtube: the invested youtube advertising budget
# sales: the observed sale values
# .fitted: the fitted sale values
# .resid: the residual errors

# RESIDUALS PLOT
ggplot(model.diag.metrics, aes(youtube, sales)) +
    geom_point() +
    stat_smooth(method = lm, se = FALSE) +
    geom_segment(aes(xend = youtube, yend = .fitted), 
                 color = "red", size = 0.3)

# DIAGNOSTICS
# Linear regression makes several assumptions about the data:

# 1. Linearity of the data. 
# The relationship between the predictor (x) and the outcome (y) 
# is assumed to be linear.

# 2. Normality of residuals. 
# The residual errors are assumed to be normally distributed.

# 3. Homogeneity of residuals variance. 
# The residuals are assumed to have a constant variance 
# (homoscedasticity)

# 4. Independence of residuals error terms.

# Potential problems include:
# 1. Non-linearity of the outcome - predictor relationships
# 2. Heteroscedasticity: Non-constant variance of error terms.
# 3. Presence of influential values in the data that can be:
#    Outliers: extreme values in the outcome (y) variable
#    High-leverage points: extreme values in the predictors (x) variable

par(mfrow = c(2, 2))
plot(model)

# 1. Residuals vs Fitted. 
# Used to check the linear relationship assumptions. 
# A horizontal line, without distinct patterns is an indication 
# for a linear relationship, what is good.

# 2. Normal Q-Q. 
# Used to examine whether the residuals are normally distributed. 
# It’s good if residuals points follow the straight dashed line.

# 3. Scale-Location (or Spread-Location). 
# Used to check the homogeneity of variance of the residuals 
# (homoscedasticity). Horizontal line with equally spread points is 
# a good indication of homoscedasticity. This is not the case in 
# our example, where we have a heteroscedasticity problem.

# 4. Residuals vs Leverage. 
# Used to identify influential cases, that is extreme values that 
# might influence the regression results when included or excluded 
# from the analysis. 

# Inspect the data
head(model.diag.metrics, 4)

# We will mainly use .fitted, .resid, .hat, .std.resid, .cooksd

# LINEARITY
# Now lets check the linearity assumption
dev.off()
plot(model,1)
# Limbani +2


# HOMOGENEITY OF VARIANCE
plot(model,3)

# RESULT
# (2) Comment for [BONUS POINT]


model2 <- lm(log(sales) ~ youtube, data = marketing)
plot(model2, 3)

# NORMALITY OF RESIDUALS
plot(model, 2)

# RESULT
# (3) Comment for [BONUS POINT]
# ... (check solutions)


# OUTLIERS AND LEVERAGE POINTS
plot(model,5)

# RESULT
# The plot above highlights the top 3 most extreme points 
# (#26, #36 and #179), with a standardized residuals below -2. 
# However, there is no outliers that exceed 3 standard deviations, 
# what is good.

# Additionally, there is no high leverage point in the data. 
# That is, all data points, have a leverage statistic below 
# 2(p + 1)/n = 4/200 = 0.02.

# INFLUENTIAL VALUES
# Value, which inclusion or exclusion can alter the results of the 
# regression analysis.

# Cook’s distance to determine the influence of a value. 
# This metric defines influence as a combination of leverage and 
# residual size.

par(mfrow = c(1, 2))
# Cook's distance
plot(model, 4)
# Residuals vs Leverage
plot(model, 5)

# By default top 3 values are labelled.
# We may change it:
dev.off()
plot(model, 4, id.n = 5)
# We can easily accees data behind these observations
model.diag.metrics %>%
    top_n(3, wt = .cooksd)

# RESULT
# The data don’t present any influential points. 
# Cook’s distance lines (a red dashed line) are not shown 
# on the Residuals vs Leverage plot because all points are 
# well inside of the Cook’s distance lines.

# EXAMPLE with potential influence points;
df2 <- data.frame(
    x = c(marketing$youtube, 500, 600),
    y = c(marketing$sales, 80, 100)
) # Bonus point goes to: Sadat Shahriar and Biswas, Dipayan
model2 <- lm(y ~ x, df2)

par(mfrow = c(1, 2))
# Cook's distance
plot(model2, 4)
# Residuals vs Leverage
plot(model2, 5)

# RESULT
# When the points are outside of the Cook’s distance, this means that 
# they have high Cook’s distance scores. In this case, the values are 
# influential to the regression results.


# PROBLEM 1
# In the data set faithful, develop a 95% prediction interval of the 
# eruption duration for the waiting time of 80 minutes.
# Determine the r^2 value.
# Determine if there is a significant relationship between the 
# variables.

attach(faithful)     # attach the data frame
# linear regression model
?faithful

eruption.lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)

# Extract the parameters of the estimated regression equations with 
# the coefficients function.

coefs = coefficients(eruption.lm)

# Now fit the eruption duration using the estimated regression 
# equation
waiting = 80
duration = coefs[1] + coefs[2]*waiting
duration

# So, if the waiting time since the last eruption has been 80 
# minutes, we can expect the next one to last 4.1762 minutes.

# ALTERNATIVE USING predict

newdata = data.frame(waiting=80) # wrap the parameter
predict(eruption.lm, newdata)

# Check attributes of the model
attributes(summary(eruption.lm))

summary(eruption.lm)$r.squared

predict(eruption.lm, newdata, interval="confidence")
# The 95% confidence interval of the mean eruption duration for the 
# waiting time of 80 minutes is between 4.1048 and 4.2476 minutes.

# Obtain the residual plot for the faithful dataset against the independent 
# variable waiting.
eruption.res = resid(eruption.lm)
dev.off()
plot(faithful$waiting, eruption.res,
     ylab="Residuals",
     xlab="Waiting Time",
     main="Old Faithful Eruptions")

abline(0, 0, col='red')
