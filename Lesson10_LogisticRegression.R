# 04/09/2021
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. https://www.tutorialspoint.com/r/r_logistic_regression.htm
# 2. https://uc-r.github.io/logistic_regression
# 3. https://osf.io/scnpe/


# PLAN
# 1. EXAMPLE 1 (Intro)
# 2. EXAMPLE 2 (Advanced)
#   - preparing data
#   - simple regression
#   - multiple regression
#   - model evaluation and diagnostics
# 3. EXAMPLE 3 (Project)
#

# The general mathematical equation for logistic regression is −
# y = 1/(1+e^-(a+b1x1+b2x2+b3x3+...))
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
?Default
default <- as_tibble(ISLR::Default)
head(default)

# Why do we use logistic regression?
# Linear regression is not appropriate in the case of a qualitative response. 
library(png)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(readPNG("/Users/apple/Desktop/6323_TA/R_scripts/Lesson10_data/plot1-1.png"),
            xleft = 0, xright = 1, ybottom = 0, ytop = 1)

# Prepare data
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, 
                 prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

model1 <- glm(default ~ balance, family = "binomial", data = train)
# In the background the glm, uses maximum likelihood to fit the model.

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

# Deviance is analogous to the sum of squares calculations in linear 
# regression and is a measure of the lack of fit to the data in a logistic 
# regression model. The null deviance represents the difference between a 
# model with only the intercept (which means “no predictors”) and a saturated 
# model (a model with a theoretically perfect fit). 

# coefficients

# the coefficient estimates from logistic regression characterize the 
# relationship between the predictor and response variable on a LOG-odds scale
tidy(model1)
# one-unit increase in balance is associated with an increase in the log 
# odds of default by 0.0057 units
exp(coef(model1))
# OR balance coefficient as - for every one dollar increase in monthly 
# balance carried, the odds of the customer defaulting increases by a factor 
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

predict(model2, data.frame(student = factor(c("Yes", "No"))), 
        type = "response")

# MULTIPLE LOGISTIC REGRESSION

# model that predicts the probability of default based on the balance, income 
# (in thousands of dollars), and student status variables.
model3 <- glm(default ~ balance + income + student, family = "binomial", 
              data = train)
tidy(model3)

# Which variable is the most influential in predicting the responce?
caret::varImp(model3)

# Predictions
new.df <- tibble(balance = 1500, income = 40, student = c("Yes", "No"))
predict(model3, new.df, type = "response")

# DIAGNOSTICS

# Likelihood Ratio Test 
anova(model1, model3, test = "Chisq")
# model3 does provide an improved model fit.

# Pseudo R^2
list(model1 = pscl::pR2(model1)["McFadden"],
     model2 = pscl::pR2(model2)["McFadden"],
     model3 = pscl::pR2(model3)["McFadden"])
# model2 has a very low value corroborating its poor fit

# Residuals
# For logistic regression:
# -residuals may not be normally distributed
# -variance may not be constant

model1_data <- augment(model1) %>% 
    mutate(index = 1:n())

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

list(
    model1 = table(test$default, test.predicted.m1 > 0.5) %>% prop.table() %>% round(3),
    model2 = table(test$default, test.predicted.m2 > 0.5) %>% prop.table() %>% round(3),
    model3 = table(test$default, test.predicted.m3 > 0.5) %>% prop.table() %>% round(3)
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

par(mfrow=c(1, 2))

prediction(test.predicted.m1, test$default) %>%
    performance(measure = "tpr", x.measure = "fpr") %>%
    plot()

prediction(test.predicted.m2, test$default) %>%
    performance(measure = "tpr", x.measure = "fpr") %>%
    plot()

# AUC - area under curve
# ranges from 0.5 to 1.0 
# values above 0.80 indicate that the model does a good job in 
# discriminating between the two categories which comprise our 
# target variable

# model 1 AUC
prediction(test.predicted.m1, test$default) %>%
    performance(measure = "auc") %>%
    .@y.values

# model 2 AUC
prediction(test.predicted.m2, test$default) %>%
    performance(measure = "auc") %>%
    .@y.values


# EAMPLE (project)
# Article-level model to facilitate measuring factors 
# relating to SA and CIP diversity



library(dplyr)
library(ggplot2)
library(rcompanion)
library(rms)
library(questionr)

#### Read data
df_article <- read.csv('/Users/apple/Desktop/6323_TA/Project/data/ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv')

#### Filters
###### Filter Year [1970-2018]
###### Filter Kp >= 2 and Wp >= 2
df_article = df_article %>% filter(Yp >= 1970)
df_article = df_article %>% filter(Yp <= 2018)
df_article = df_article %>% filter(Kp >= 2)
df_article = df_article %>% filter(nMeSHMain >= 2)
df_article = df_article %>% filter(IRegionRefinedp > 0 & IRegionRefinedp < 7)

#### Convert Data types
df_article$eidsp = as.factor(df_article$eidsp)
df_article$Yp = as.integer(df_article$Yp)
df_article$Kp = as.integer(df_article$Kp)
df_article$MeanZJp = as.double(df_article$MeanZJp)
df_article$XSAp = as.factor(df_article$XSAp)
df_article$XCIPp = as.factor(df_article$XCIPp)
df_article$NRegp = as.integer(df_article$NRegp)
df_article$NSAp = as.integer(df_article$NSAp)
df_article$NCIPp = as.integer(df_article$NCIPp)
df_article$nMeSHMain = as.integer(df_article$nMeSHMain)
df_article$IRegionRefinedp = as.factor(df_article$IRegionRefinedp)

## Model 1 - for X_SA
options(scipen=2)
model1 <- glm(XSAp ~ Yp + MeanZJp + log(Kp) + log(nMeSHMain) + NRegp + NCIPp, 
              data = df_article, family=binomial(link='logit'))

# Here:
# XSAp: binary indicator variable = 1 if any 2+ SA are present, and 0 otherwise
# Yp: article’s publication year
# MeanZJp: Journal’s mean Zp value calculated across all its articles 
# Kp: article’s coauthor count based upon author list in PubMed record
# NRegp: article’s count variable indicating the total number of regions 
# NCIPp: article’s count variable indicating the total number of CIP 

summary(model1)
?nagelkerke
nagelkerke(model1)

?odds.ratio
output = odds.ratio(model1) # HEAVY COMPUTATIONAL!
output = apply(output, 2, formatC, format="f", digits=4)
output