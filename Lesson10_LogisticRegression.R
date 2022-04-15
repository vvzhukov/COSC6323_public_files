# Created 04/09/2021
# Updated 04/08/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. https://www.tutorialspoint.com/r/r_logistic_regression.htm
# 2. https://uc-r.github.io/logistic_regression
# 3. https://osf.io/scnpe/


# PLAN
# 1. EXAMPLE 1 (Intro)
# 2. EXAMPLE 2 (Advanced)
#   - simple regression
#   - multiple regression
#   - interactions in logistic regression

# The general mathematical equation for logistic regression is −
# y = e^(a+b1x1+b2x2+b3x3+...)/(1+e^(a+b1x1+b2x2+b3x3+...))
# y is the response variable.
# x is the predictor variable.
# a and b are the coefficients which are numeric constants.

# We will use glm() function to create model
# SYNTAX: glm(formula,data,family)
# formula - the symbol presenting the relationship between the variables.
# data - the data set giving the values of these variables.
# family - R object to specify the details of the model. It's value is 
#   binomial for logistic regression.

# EXAMPLE 1
# Select some columns form mtcars.
mtdata <- mtcars[,c("am","cyl","hp","wt")]
print(head(mtdata))

am.data = glm(formula = am ~ cyl + hp + wt, data = mtdata, family = binomial)
print(summary(am.data))

# In the summary as the p-value in the last column is more than 0.05 for 
# the variables "cyl" and "hp", we consider them to be insignificant in 
# contributing to the value of the variable "am". Only weight (wt) impacts 
# the "am" value in this regression model.


# EXAMPLE 2 (advanced)
# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs
library(caret)
library(pscl)

# Load data 
# Data for predicting default

default <- as_tibble(ISLR::Default)
head(default)

# Why do we use logistic regression?
# Linear regression is not appropriate in the case 
# of a qualitative response. 

library(png)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(readPNG("/Users/apple/Desktop/6323_TA/Git_code/Lesson10_data/plot1-1.png"),
            xleft = 0, xright = 1, ybottom = 0, ytop = 1)

# Prepare data
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, 
                 prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

model1 <- glm(default ~ balance, family = "binomial", data = train)
# In the background the glm, uses maximum likelihood to fit 
# the model.

default %>%
    mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
    ggplot(aes(balance, prob)) +
    geom_point(alpha = .15) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggtitle("Logistic regression model fit") +
    xlab("Balance") +
    ylab("Probability of Default")

# We can access summary of the model
summary(model1)

# Deviance is analogous to the sum of squares calculations in 
# linear regression and is a measure of the lack of fit to the 
# data in a logistic regression model. The null deviance 
# represents the difference between a model with only the
# intercept (which means “no predictors”) and a saturated 
# model (a model with a theoretically perfect fit). 

# coefficients

# the coefficient estimates from logistic regression 
# characterize the relationship between the predictor 
# and response variable on a LOG-odds scale
tidy(model1)
# one-unit increase in balance is associated with an 
# increase in the log odds of default by 0.0057 units
exp(coef(model1))
# OR balance coefficient as - for every one dollar 
# increase in monthly balance carried, the odds of 
# the customer defaulting increases by a factor 
# of 1.0057

# Similar to LM we may calculate conf intervals:
confint(model1)

# Predictions
# compare the probability of defaulting based on balances of $1000 and $2000.
predict(model1, data.frame(balance = c(1000, 2000)), type = "response")
# the probability of defaulting increases signficantly, from 0.5% to 58%!

# We use qualitative predictors with the logistic regression model. 
# As an example, we can fit a model that uses the student variable.

model2 <- glm(default ~ student, family = "binomial", data = train)
tidy(model2)
# The coefficient associated with student = Yes is positive, and the 
# associated p-value is statistically significant. This indicates that 
# students tend to have higher default probabilities than non-students.

# This model suggests that a student has nearly twice the odds of 
# defaulting than non-students
exp(coef(model2))

predict(model2, data.frame(student = factor(c("Yes", "No"))), 
        type = "response")

# MULTIPLE LOGISTIC REGRESSION

# model that predicts the probability of default based on the balance, income 
# (in thousands of dollars), and student status variables.
model3 <- glm(default ~ balance + income + student, family = "binomial", 
              data = train)
tidy(model3)

model4 <- glm(default ~ log(balance+1) + log(income+1) + student, family = "binomial", 
              data = train)
summary(model4)
# Which variable is the most influential in predicting the responce?
caret::varImp(model3)

# Predictions
new.df <- tibble(balance = 1500, income = 40000, student = c("Yes", "No"))
predict(model3, new.df, type = "response")

# DIAGNOSTICS

# Likelihood Ratio Test 
anova(model1, model3, test = "Chisq")
# model3 does provide an improved model fit.
anova(model3, model4, test = "Chisq")

# Pseudo R^2 as an alternative to the R^2
# 1 - LN(m1)/LN(m0)
list(model1 = pscl::pR2(model1)["McFadden"],
     model2 = pscl::pR2(model2)["McFadden"],
     model3 = pscl::pR2(model3)["McFadden"],
     model4 = pscl::pR2(model4)["McFadden"])
# model2 has a very low value corroborating its poor fit

# Residuals
# For logistic regression:
# -residuals may not be normally distributed
# -variance may not be constant

model1_data <- augment(model1) %>% 
    mutate(index = 1:n())
head(model1_data)
ggplot(model1_data, aes(index, .std.resid, color = default)) + 
    geom_point(alpha = .5) +
    geom_ref_line(h = 3)
# Those standardized residuals that exceed 3 represent possible 
# outliers and may deserve closer attention.
# We can filter for these residuals to get a closer look:
model1_data %>% 
    filter(abs(.std.resid) > 3)

# Cook's distance
plot(model1, which = 4, id.n = 5)
#  the top five influential points include:
# - those customers who defaulted with very low balances
# - two customers who did not default, yet had balances over $2,000

model1_data %>% 
    top_n(5, .cooksd)

# Validation of predicted values
test.predicted.m1 <- predict(model1, newdata = test, type = "response")
test.predicted.m2 <- predict(model2, newdata = test, type = "response")
test.predicted.m3 <- predict(model3, newdata = test, type = "response")
test.predicted.m4 <- predict(model4, newdata = test, type = "response")


list(
    model1 = table(test$default, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3),
    model2 = table(test$default, test.predicted.m2 > 0.5) %>% prop.table() %>% round(3),
    model3 = table(test$default, test.predicted.m3 > 0.5) %>% prop.table() %>% round(3),
    model4 = table(test$default, test.predicted.m4 > 0.5) %>% prop.table() %>% round(3)
)

# We don’t see much improvement between models 1 and 3 and although model 2 
# has a low error rate don’t forget that it never accurately predicts 
# customers that actually default

test %>%
    mutate(m1.pred = ifelse(test.predicted.m1 > 0.5, "Yes", "No"),
           m2.pred = ifelse(test.predicted.m2 > 0.5, "Yes", "No"),
           m3.pred = ifelse(test.predicted.m3 > 0.5, "Yes", "No")) %>%
    summarise(m1.error = mean(default != m1.pred),
              m2.error = mean(default != m2.pred),
              m3.error = mean(default != m3.pred))

# ROC receiving operating characteristic
# visual measure of classifier performance


library(ROCR)
# visual measure of classifier performance

# Using the proportion of positive data points 
# that are correctly considered as positive and 
# the proportion of negative data points that are 
# mistakenly considered as positive, we generate 
# a graphic that shows the trade off between the 
# rate at which you can correctly predict something 
# with the rate of incorrectly predicting something


par(mfrow=c(1, 2))
prediction(test.predicted.m1, test$default) %>%
    performance(measure = "tpr", x.measure = "fpr") %>%
    plot()
prediction(test.predicted.m2, test$default) %>%
    performance(measure = "tpr", x.measure = "fpr") %>%
    plot()

# AUC - area under curve
# ranges from 0.5 to 1.0 
# values above 0.80 indicate that the model does a 
# good job in discriminating between the two 
# categories which comprise our target variable

# Visually we may tell that model #1 performed
# better

# model 1 AUC
prediction(test.predicted.m1, test$default) %>%
    performance(measure = "auc") %>%
    .@y.values

# model 2 AUC
prediction(test.predicted.m2, test$default) %>%
    performance(measure = "auc") %>%
    .@y.values

# More model improvements should give you better 
# results

# INTERACTIONS IN LOGISTIC REGRESSION
Berkeley = data_frame('Gender' = rep(c('m','f'), 6),
                      'Dept' = c('A', 'A','B', 'B', 'C', 'C', 'D', 'D', 'E', 'E', 'F', 'F'),
                      'Yes' = c(512, 89, 353, 17, 120, 202, 138, 131, 53, 94, 22, 24),
                      'No' = c(313, 19, 207, 8, 205, 391, 279, 244, 138, 299,351, 317))

full = glm(cbind(No,Yes) ~ Dept*Gender,family=binomial,data=Berkeley)
summary(full)

