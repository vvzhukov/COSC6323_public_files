# Created 04/09/2021
# Updated 03/31/2023
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. https://www.tutorialspoint.com/r/r_logistic_regression.htm
# 2. https://uc-r.github.io/logistic_regression
# 3. https://osf.io/scnpe/
# 4. https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
# 5. https://quantifyinghealth.com/linear-regression-with-interaction-in-r/
# 6. https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
# 7. https://libguides.princeton.edu/R-Panel
# 8. https://janajarecki.com/blog/repeated-measures-regression-in-r/

# PLAN
# - fixed effects/random effects
# - interactions
# - repeated measures design

# Panel data (also known as longitudinal or cross-sectional
# time-series data) refers to data for n different 
# entities at different time periods. These entities 
# could be states, companies, individuals, countries, etc

# Here is our sample
# Variables of interest:
# Country
# Year
# y - dependent variable
# x1 - independent variable

library(foreign)
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type="b", data=Panel)

library(car)
scatterplot(y~year|country, boxplots=FALSE, 
            smooth=TRUE, reg.line=FALSE, data=Panel)

plotmeans(y ~ country, 
          main="Heterogeineity across countries", 
          data=Panel)

plotmeans(y ~ year, 
          main="Heterogeineity across years", 
          data=Panel)


# Regular OLS 
# regression does not consider heterogeneity across groups or time
ols <-lm(y ~ x1, data=Panel) 
summary(ols)

yhat <- ols$fitted
plot(Panel$x1, Panel$y, pch=19, xlab="x1", ylab="y")
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

yhat <- fixed.dum$fitted
scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

# Each component of the factor variable (country) is absorbing 
# the effects particular to each country. Predictor x1 was not 
# significant in the OLS model, once controlling for differences 
# across countries, x1 became significant in the OLS_DUM 
# (i.e. LSDV model).



# FE: remove the effect of those time-invariant 
# characteristics so we can assess the net effect 
# of the predictors on the outcome variable.

# RE: If the individual effects are strictly uncorrelated 
# with the regressors it may be appropriate to model 
# the individual specific constant terms as randomly 
# distributed across cross-sectional units.

library(plm)
#fixed model
fixed <- plm(y ~ x1, data=Panel, 
             index=c("country", "year"), #panel settings
             model="within")  #fixed effect options

#random model
random <- plm(y ~ x1, data=Panel, 
              index=c("country", "year"), 
              model="random")  

# Hausman test
# The test evaluates the consistency of an estimator 
# when compared to an alternative, less efficient 
# estimator which is already known to be consistent

phtest(fixed,random) # Hausman test
# p < 0.05 - use fixed effect model
# alternatively use random effect


# FE Fixed effect
summary(fixed)
# (n = # of groups/panels, T = # years, N = total # of observations)

# The coeff of x1 indicates how much Y changes overtime, 
# on average per country, when X increases by one unit.

# The first p-value indicates whether the variable has 
# a significant influence on your dependent 
# variable (y). If p value smaller than 0.05 then yes. 
# x1 being a significant variable. 

# The second p-value indicates the quality of the model. 
# In this model we have only one variable so the p-value 
# is exactly the same.

# We may display fixed effects (constants for each country)
fixef(fixed)


# RE: Random effect
summary(random)
# include both the within-entity and between-entity effects

# Time-series–cross-section (TSCS) data represents the 
# average effect of X over Y when X changes across time 
# and between countries by one unit.

# Here p-value test the hypothesis that each 
# coefficient is different from 0
# Lets compare simple OLS model with fixed-effects model
summary(ols)

# We may also use F test to compare these two models
# Null here would be that OLS is better than fixed
pFtest(fixed, ols)

# Fixed-effect model would be a better fit.
# Instead of using plm() function we may use 
# dummy variables:
fixed.dum <-lm(y ~ x1 + factor(country) - 1, data=Panel)
# here '- 1' used to remove the Intercept

summary(fixed.dum)
# We can see that the estimate for x1 is the same with that of the 
# fixed effects model using plm. The constants for each country is 
# shown in the result as well.


# TEST for time-fixed effects
# construct a time-fixed effects model
fixed.time <- plm(y ~ x1 + factor(year), data=Panel, 
                  index=c("country","year"), 
                  model="within")
summary(fixed.time)
# now we may compare fixed-time effects model with fixed model:
pFtest(fixed.time, fixed)
# Lagrange Multiplier Test for time effects (Breush-Pagan)
plmtest(fixed, c("time"), type=("bp"))
# Large p-value indicates that there is no significance from adding
# time-fixed effects


# Random effects vs Pooled OLS
# The LM test helps you decide between a random effects regression 
# and a simple OLS regression.

# The null hypothesis in the LM test is that variances across 
# entities is zero. This is, no significant difference across 
# units (i.e. no panel effect).
pool <- plm(y ~ x1, data=Panel, model="pooling")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null 
# is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp"))


# Cross-sectional dependence testing
# cross-sectional dependence is a problem in macro panels 
# with long time series. This is not much of a problem in 
# micro panels (few years and large number of cases).

# The null hypothesis in the B-P/LM and Pasaran CD 
# tests of independence is that residuals across entities
# are not correlated. B-P/LM and Pasaran CD 
# (cross-sectional dependence) tests are used to test 
# whether the residuals are correlated across entities. 
# Cross-sectional dependence can lead to bias in tests 
# results (also called contemporaneous correlation).

# The null is that there is not cross-sectional dependence
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))
# Because p-value > 0.05, we conclude that there is NO 
# cross-sectional dependence


# Serial correlation testing
# Serial correlation tests apply to macro panels with long 
# time series. Not a problem in micro panels (with very few years).

# The null is that there is not serial correlation.
pbgtest(fixed)
# Because p-value > 0.05, we conclude that there is NO serial correlation


# Unit roots/stationarity testing
# The Dickey-Fuller test to check for stochastic trends.
# The null hypothesis is that the series has a unit root (i.e. non-stationary)
# If unit root is present you can take the first difference of the variable.
adf.test(Panel$y, k=2)
# p-value < 0.05, we conclude that the series does NOT have unit root. 
# In other words, the series is stationary


# Heteroskedasticity testing
# The null hypothesis for the Breusch-Pagan test is homoskedasticity
bptest(y ~ x1 + factor(country), data = Panel, studentize=F)
# Because p-value < 0.05, we detect hetersokedasticity
# If hetersokedasticity is detected we need to use a robust 
# covariance matrix (Sandwich estimator) to account for it

# Controlling for heteroskedasticity: Random effects
# The –vcovHC– function estimates three heteroskedasticity-consistent 
# covariance estimators:
    
# "white1" - for general heteroskedasticity but no serial correlation. 
# Recommended for random effects.
# "white2" - is “white1” restricted to a common variance within groups. 
# Recommended for random effects.
# "arellano" - both heteroskedasticity and serial correlation. 
# Recommended for fixed effects.

# The following options apply:
# HC0 - heteroskedasticity consistent. The default.
# HC1,HC2, HC3 – Recommended for small samples. HC3 gives less weight 
# to influential observations.
# HC4 - small samples with influential observations 
# HAC - heteroskedasticity and autocorrelation consistent 
# (type ?vcovHAC for more details)

# Original coefficients
?coeftest
coeftest(random) 

# Heteroskedasticity consistent coefficients
coeftest(random, vcovHC) 

# Heteroskedasticity consistent coefficients, type 3
coeftest(random, vcovHC(random, type = "HC3")) 

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), 
         function(x) sqrt(diag(vcovHC(random, type = x)))))


# Controlling for heteroskedasticity: Fixed effects
# Original coefficients
coeftest(fixed)

# Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC)

# Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, method = "arellano"))

# Heteroskedasticity consistent coefficients, type 3
coeftest(fixed, vcovHC(fixed, type = "HC3"))

# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), 
         function(x) sqrt(diag(vcovHC(fixed, type = x)))))


# INTERACTIONS
# Couple more words about the interaction
# creating a data frame from the iris data set
dat = data.frame(X = iris$Petal.Length,
                 Y = iris$Sepal.Length,
                 Z = iris$Petal.Width)
# linear regression model with interaction between X and Z
summary(lm(Y ~ X + Z + X:Z, data = dat))
# or
summary(lm(Y ~ X*Z, data = dat))
# So, the linear regression equation is:
# Y = 4.58 + 0.44 X - 1.24 Z + 0.19 X×Z

# The intercept, 4.58, is the Y value when X = 0 and Z = 0.

# The coefficient of X, 0.44, is the change in Y associated 
# with a 1 unit increase in X, when Z = 0. If Z = 0 is implausible, 
# then the effect of X on Y can be interpreted as follows: 
# A 1 unit increase in X changes Y by: 0.44 + 0.19 Z. 
# We can plug in different values of Z to get the effect of X on Y.

# The coefficient of Z, -1.24, is the change in Y associated with 
# a 1 unit increase in Z, when X = 0. If X = 0 is implausible, 
# then the effect of Z on Y can be interpreted as follows: A 1 
# unit increase in Z changes Y by: -1.24 + 0.19 X. We can plug 
# in different values of X to get the effect of Z on Y.

# The coefficient of the interaction between X and Z, 0.19, is 
# the increase of effectiveness of X on Y for a 1 unit increase 
# in Z. Or vice-versa, 0.19 is the increase of effectiveness 
# of Z on Y for a 1 unit increase in X.

# How to decide if the model with interaction is better than 
# the model without interaction?

# 1. Look at the p-value associated with the coefficient of the 
# interaction term:
# In our case, the coefficient of the interaction term is 
# statistically significant. This means that there is strong 
# evidence for an interaction between X and Z.

# 2. Compare the R-squared of the model without interaction to 
# that of the model with interaction:

summary(lm(Y ~ X + Z, data = dat))$r.squared
summary(lm(Y ~ X + Z + X:Z, data = dat))$r.squared
# This means that the interaction X×Y explains 4.2% of the 
# variance in Y. Which is a substantial effect!

# Visualise:
plot_model(lm(Y ~ X*Z, data = dat), type='int')    
    

# REAPETED MEASURES
# same subjects are measured more than once
# multiple data points from each participant

simple.df <- read.csv('https://raw.githubusercontent.com/JanaJarecki/hp/master/sample_data.csv')
head(simple.df)

# start with a simple regerssion
library(lme4)
lm(answer ~ question, data = simple.df)
# the standard regression knows no clusters. 
# It always assumes that one intercept and one slope fits them all. 

# 1. Intercepts varying per group
# The first regression, which accounts for grouping/repeated measures, 
# models different intercepts but assumes one slope to fit them all. 
model_in <- lmer(answer ~ question + (1|participant), data=simple.df)
summary(model_in)

# 2. Slopes varying per group
# accounts for grouping/repeated measures, allows just different 
# slopes while assuming one intercept to fit them all. 
model_sl <- lmer(answer ~ question + (question-1|participant), data=simple.df)
summary(model_sl)

# 3. Slopes and intercepts varying per group
# accounts for grouping/repeated measures, allows both intercepts 
# and slopes to differ between participants.
model_in_sl <- lmer(answer ~ question + (1+question|participant), data=simple.df)
summary(model_in_sl)

# You can also add other predictors, like age:
lmer(answer ~ age + question + (1 + question | participant), data=simple.df)
# the model is sometimes called a mixed model

# Compare the models:
anova(model_in, model_sl, model_in_sl)




# EXTRA
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

