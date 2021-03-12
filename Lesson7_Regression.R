# 03/11/2021
# Vitalii Zhukov
# COSC 6323
# Ref.: https://www.tutorialspoint.com/r/r_linear_regression.htm

# EXAMPLE 1

# PREDICTOR
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# RESPONCE
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)

print(relation)

print(summary(relation))


# PREDICT FUNCTION

# The predictor vector.
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# The resposne vector.
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

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

# Load the data
# predicting sales units on the basis of the amount of money 
# spent in the three advertising medias
data("marketing", package = "datarium")

# Inspect the data
sample_n(marketing, 3)


# Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales %>%
    createDataPartition(p = 0.8, list = FALSE)

train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]

# Build the model
model <- lm(sales ~ youtube, data = train.data)
summary(model)$coef

# Make predictions
newdata <- data.frame(youtube = c(0,  1000))
model %>% predict(newdata)


# PROBLEM 1
# In the data set faithful, develop a 95% prediction interval of the 
# eruption duration for the waiting time of 80 minutes.
# Determine the r2 value.
# Determine if there is a significant relationship between the 
# variables.

attach(faithful)     # attach the data frame
# linear regression model

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


# PROBLEM 2
# Obtain the residual plot for the faithful dataset against the independent 
# variable waiting.
eruption.res = resid(eruption.lm)

plot(faithful$waiting, eruption.res,
     ylab="Residuals",
     xlab="Waiting Time",
     main="Old Faithful Eruptions")

abline(0, 0)
