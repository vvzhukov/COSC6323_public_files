# Created 03/25/2021
# Updated 03/25/2022
# Vitalii Zhukov
# COSC 6323

# Ref.: 
# https://www.tutorialspoint.com/r/r_multiple_regression.htm
# https://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr_interact.html
# https://www.rdocumentation.org/packages/GGally/versions/1.5.0/topics/ggpairs
# https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# https://www.statmethods.net/stats/regressi
# http://statweb.stanford.edu/~jtaylo/courses/stats203/notes/diagnostics.pdf
# http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
# https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
# https://quantifyinghealth.com/stepwise-selection/

# PLAN

# 1. Multiple regression (mtcars)
# 2. Variables selection (criterion & all combinations)
# 3. Variables selection (forward, backward, stepwise)
# 4. Different package


# 1. Mtcars multiple regression

# Establish the relationship between "mpg" as a response variable 
# with "disp","hp" and "wt" as predictor variables
?mtcars
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))

# Create Relationship Model & get the Coefficients
model <- lm(mpg~disp+hp+wt, data = input)
model2 <- lm(mpg~wt, data = input)
# Show the model.
summary(model2)
summary(model)

?AIC
?BIC
# Smaller AIC or BIC => better the fit
AIC(model)
AIC(model2)
BIC(model)
BIC(model2)

# Another way to get coefficients
print(model)

# Get the Intercept and coefficients as vector elements.
coef(model)

Interc <- coef(model)[1]
Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

# For a car with disp = 221, hp = 102 and wt = 2.91 the 
# predicted mileage is
x1 <- 221
x2 <- 102
x3 <- 2.91
Interc + Xdisp*x1 + Xhp*x2 + Xwt*x3

# Matrix scatterplot:
plot(input, pch=16, col="blue",
     main="Matrix Scatterplot of mpg, disp, hp, and wt")

# Diagnostic plots for the model
par(mfrow=c(2, 2))
plot(model)

# What can go wrong?
# Regression function can be wrong - missing predictors, nonlinear
# - True regression function may have higher-order non-linear terms: 
#    X1^2 or even X1*X2
#    How to fix? Difficult in general. 
#    Check "added variable" and "partial residual"

# Assumptions about the errors can be wrong.
# - Errors not normally distributed (check QQ)
# - Variance may not be constant. (sometimes transformations help)
# - Errors may not be independent. This can affect the t, F stats.

# Outliers & Influential observations: both in predictors and observations
# - some residuals may be much longer than others => evidence of an outlier 
# - we may change influence by dropping each observation and check the model



# 2. Variables selection (criterion & all combinations)

## Criterion

##  (largest) R^2: Goodness of fit
##  (lowest) AIC: Akaike Information Criteria 
##  (lowest) SBIC: Sawa's Bayesian Information Criteria 
##  (lowest) SBC: Schwarz Bayesian Criteria 
##  (lowest) MSEP: Estimated error of prediction, assuming multivariate normality 
##  (lowest) FPE: Final Prediction Error 
##  (lowest) HSP: Hocking's Sp (adjustment of the residual sum of Squares)
##  (lowest) APC: Amemiya Prediction Criteria

# All possible subsets of the set of potential variables
# If we have K independent variables => 2^K distinct subsets

library(olsrr)
model3 <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model3)
k
plot(k) # goes to quartz

# Select the subset of predictors that do the best at meeting 
# some criterion, such as having the largest R2 value or 
# the smallest MSE, Mallow’s Cp or AIC.
?ols_step_best_subset
# We can also check the best subsets picked automatically
k <- ols_step_best_subset(model)
plot(k) # goes to quartz



# 3. Variables selection (forward, backward, stepwise)

# Stepwise forward algorithm
# - Begins with a model that contains no variables 
#       (called the Null Model)
# - Then starts adding the most significant variables 
#       one after the other
# - Until a pre-specified stopping rule is reached 
#       or until all the variables under consideration 
#       are included in the mode

# How to: choose most significant variable:
# It has the smallest p-value, or
# It provides the highest increase in R^2, or
# It provides the highest drop in model RSS 

data(surgical)
?surgical

# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model) 
ols_step_forward_p(model, details = TRUE)


# Stepwise backward algorithm

# - Begins with a model that contains all variables 
#       under consideration (called the Full Model)
# - Then starts removing the least significant 
#       variables one after the other
# - Until a pre-specified stopping rule is reached 
#       or until no variable is left in the model

# How to: determine the least significant variable

# Has the highest p-value in the model, or
# Its elimination from the model causes the lowest 
#    drop in R2, or
# Its elimination from the model causes the lowest 
#   increase in RSS (Residuals Sum of Squares) 

# stepwise backward regression
ols_step_backward_p(model)
ols_step_forward_aic(model)

ols_step_backward_p(model, details = TRUE)

# Which to choose?
# Use forward when
# - when the number of variables under consideration 
#           is very large (could be larger than n)
# Use backward when
# - you need to consider affect of all variables 
#           simultaneously (in case of colinearity)

# General rule:
# Unless the number of candidate variables > n, 
#       use a backward stepwise approach.


# Mixed selection or Stepwise selection

# Build regression model from a set of candidate 
# predictor variables by entering and removing 
# predictors based on p values, in a stepwise manner 
# until there is no variable left to enter or remove 
# any more. The model should include all the candidate 
# predictor variables.
ols_step_both_aic(model)
ols_step_both_aic(model, details = TRUE)


# 4. Using packages
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

data(swiss)
?swiss

# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)

# Stepwise regression model
# here direction could be: forward, backward and both
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = TRUE)

stepAIC(model, direction = "forward", 
        trace = TRUE)

summary(step.model)

# we can limit the number of predictors using nvmax
# and methods as “backward”, “forward” and “seqrep”
?regsubsets
models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "seqrep")
summary(models)
par(mfrow=c(1, 1))
plot(models,scale="adjr2")

# 5. Additional methods (not covering today)
# based on Akaike Information Criteria:
#   Stepwise AIC Forward Regression
#   Stepwise AIC Backward Regression
#   Stepwise AIC Regression


