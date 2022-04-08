# Created: 04/01/2021
# Modified: 04/01/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/Benchmarks/LinearMixedModels_JDS_Dec2010.pdf
# https://stats.stackexchange.com/questions/94010/understanding-dummy-manual-or-automated-variable-creation-in-glm
# https://www.jaredknowles.com/journal/2014/5/17/mixed-effects-tutorial-2-fun-with-mermod-objects
# https://rpubs.com/mlmcternan/BC-lme
# http://jakewestfall.org/misc/Winter2014.pdf
# https://bookdown.org/carillitony/bailey/chp6.html

# PLAN
# 0. Dummy variables
#   CHAR VECTORS
#   INT AND NUM VECTORS
#   FACTOR MANIPULATION
#   DUMMY INTERACTION
# EXTRA
# 0. GLM vs LM
# 1. Fit the Non-Multilevel Models
# 2. GLM
# 3. Fit a varying intercept model
# 4. Exploring Random Slopes
# 5. Fit a varying intercept model with lmer
# 6. Fit a varying slope model with lmer
# 7. Review exercise

# 0. Dummy variables
library(tidyverse)
library(magrittr)
library(broom)
library(estimatr)
library(forcats)
library(janitor)
library(DT)

# CHAR VECTORS as dummy vars
# R uses factors to deal with categorical variables

?UCBAdmissions

ucb_admissions <- UCBAdmissions %>% 
    as_tibble() %>%  
    janitor::clean_names() %>% 
    glimpse()

# If we use Admit as an argument in lm, R will correctly 
# treat Admit as single dummy variable with two categories

ucb_admissions %>%
    lm(n ~ admit, .)

# R has coded Rejected as 1 and Admitted as 0. The regression 
# indicates that mean of admitted is 146.25 while the mean 
# number rejected is 230.92. We can confirm that directly as well.

ucb_admissions %>% 
    group_by(admit) %>% 
    summarize(Average = mean(n))

# Similarly, if we want to calculate the mean number of applicants 
# by department, R will treat Dept as 5 dummy variables.

ucb_admissions %>% 
    lm(n ~ dept, .)

# The mean number of applicants in Department A is 233.25. 
# To find the mean number of applicants for each department 
# add the appropriate coefficient to 233.25.

# Question: What is the mean number of applications for the deptD?

# Differences in Mean

# Number of applications by Gender (alpha = 0.05)
# We will try following approaches:
# 1. Use t.test for class 'formula`
# 2. Use lm or lm_robust

# Assume equal variances
ucb_admissions %>%
    t.test(n ~ gender, ., var.equal = TRUE)
ucb_admissions %>%
    lm(n ~ gender, .) %>% 
    tidy()

# Assume unequal variances
ucb_admissions %>%
    t.test(n ~ gender, .)
ucb_admissions %>%
    lm_robust(n ~ gender, .) %>% 
    tidy()


# INT AND NUM VECTORS as dummy vars

mtcars %>% 
    glimpse()

# check mpg for automatic and manual transmission
mtcars %>%
    lm_robust(mpg ~ am, .) %>% 
    tidy()

# but what should we do if the var is not coerced by 0 and 1?
# Let's check mpg for different cylinders cars (4,6,8):
mtcars %>%
    lm(mpg ~ as.factor(cyl), .) %>% 
    summary()
# At least 1 is different

# The base case is cars with 4 cylinders with an average 
# mpg of 26.7 mpg. 6 cylinder cars average a statistically 
# significant 6.9 mpg less than 4 cylinder cars. 8 cylinder 
# cars average a statistically significant 11.6 mpg less than 4 
# cylinder cars. These averages are statistically 
# significantly different.

# The same without factorisation:
mtcars %>% 
    lm(mpg ~ cyl, .) %>% 
    tidy()

# β = −2.88 tells us that for each additional cylinder 
# fuel mileage will fall by 2.88 mpg.

# FACTOR MANIPULATION
# Need to: 
#   renaming factors, re-ordering factors, combining factors, etc
# Use forcats package

### Coerce cyl to a factor
mtcars$cyl %<>% 
    as.character() %>% # forcats will not coerce integer or numeric vectors to factors
    as_factor()
mtcars$cyl %>% str()

# Alternative:
remove(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
str(mtcars$cyl)
# different order!

mtcars %>% 
    lm(mpg ~ cyl, .) %>% 
    tidy()
# This model indicates that cars with 6 cylinder engines 
# average 19.74 mpg, cars with 4 cylinders average 6.9 mpg 
# more than cars with 6 cylinders, and cars with 8 cylinders 
# average 4.64 mpg less than cars with 6 cylinders. 
# Suppose, instead, you’d prefere 4 cylinder cars to be 
# the base case. 

# fct_revel changes the order of a factor by hand.
mtcars %>%
    lm(mpg ~ fct_relevel(cyl, levels = c("4", "6", "8")), .) %>% 
    tidy()

# We can permanently re-level cylinders
mtcars %>% 
    mutate(cyl = fct_relevel(cyl, "6", after = 1)) %>% 
    lm(mpg ~ cyl, .) %>% 
    tidy()

# More information regarding factor reordering:
# https://forcats.tidyverse.org/reference/fct_relevel.html

# Now lets suppose we want to change coding for the transmission
# variable 'am'

mtcars %>% 
    mutate(am = factor(am, levels = c(0,1), 
                       labels = c("automatic", "manual"))) %>% 
    DT::datatable()

# Lets go back to the model mpg = β0 + β1*am
mtcars %>% 
    mutate(am = factor(am, levels = c(0,1), 
                       labels = c("automatic", "manual"))) %>% 
    lm_robust(mpg ~ am, .) %>% 
    tidy()


# DUMMY INTERACTION variables
# variable interaction 'a*b'
# We will estimate the model
# mpg = β0 + β1*am + β2*hp + β3*am*hp + e
mtcars %>% 
    mutate(am = factor(am, levels = c(0,1), 
                       labels = c("automatic", "manual"))) %>% 
    lm_robust(mpg ~ hp*am, .) %>% 
    tidy()
lm(mpg ~ hp*am, data=mtcars)
# Note: you do not need to explicitly show β1 and β2
# in the equation. 
# R checks the dummy variable and the interactions
# In case you want just the interaction:
# mpg = β0 + β1*am*hp + e

mtcars %>% 
    lm_robust(mpg ~ I(hp*am), .) %>% 
    tidy()
# I() is used to inhibit the interpretation of operators 
# in formulas, so they are used as arithmetic operators




# EXTRA
# lm – Used to fit linear models
# glm – Used to fit generalized linear models
# family: The statistical family to use to fit the model.
# Default is gaussian but other options include binomial, 
# Gamma, and poisson among others.

# If you use lm() or glm() to fit a linear regression model, 
# they will produce the exact same results

lm(mpg ~ hp*am, data=mtcars)
?glm
glm(mpg ~ hp*am, data=mtcars)

# However you may modify glm to fit more complex models
# logistic regression
# poisson regression

lme4::lmer()
?lmer



#fit multiple linear regression model
model <- lm(mpg ~ disp + hp, data=mtcars)
#view model summary
summary(model)

#fit multiple linear regression model
model <- glm(mpg ~ disp + hp, data=mtcars)
#view model summary
summary(model)

#fit logistic regression model (next class topic)
model <- glm(am ~ disp + hp, data=mtcars, family=binomial)
#view model summary
summary(model)

#fit Poisson regression model
model <- glm(am ~ disp + hp, data=mtcars, family=poisson)
#view model summary
summary(model)

# Level coding in R = contr.treatment
?options
?contrasts
?C
# More information about reference level coding
# https://stats.stackexchange.com/a/21292/7290

# To suppress the intercept, you modify your formula by 
# adding -1 or +0 like so: y~... -1 or y~... +0.

set.seed(1)
y    = c(rnorm(30), rnorm(30, mean=1))

sex  = rep(c("Female", "Male"), each=30)
fem  = ifelse(sex == "Female", 1, 0)
male = ifelse(sex == "Male", 1, 0)

ref.level.coding.model   = lm(y~sex)
level.means.coding.model = lm(y~fem+male+0)

summary(ref.level.coding.model)
summary(level.means.coding.model)

# 1. Fit the Non-Multilevel Models

#install.packages("lme4")
#install.packages("Matrix")
#install.packages("arm")
library(lme4)  # load library
library(arm)  # convenience functions for regression in R
library(Matrix)
lmm.data <- read.table("/Users/apple/Desktop/6323_TA/Git_code/Lesson9_data/lmm.data.txt",
                       header=TRUE, sep=",", 
                       na.strings="NA", 
                       dec=".", 
                       strip.white=TRUE)

# Fictional data where the interval scaled outcome variable 
# Extroversion (extro) is predicted by fixed effects for the 
# interval scaled predictor Openness to new experiences (open), 
# the interval scaled predictor Agreeableness (agree), the 
# interval scaled predictor Social engagement (social), and the 
# nominal scaled predictor Class (class); as well as the 
# random (nested) effect of Class within School (school). 
# The data contains 1200 cases evenly distributed among 24 
# nested groups (4 classes within 6 schools).

#summary(lmm.data)
head(lmm.data)

# Here we have data on the extroversion of subjects nested 
# within classes and within schools.

# Let's start by fitting a simple OLS regression..
OLSexamp <- lm(extro ~ open + agree + social, data = lmm.data)
display(OLSexamp)

# 2. GLM. 
# So far this model does not fit very well at all..
# Lets try generalized lm - glm
MLexamp <- glm(extro ~ open + agree + social, data=lmm.data)
display(MLexamp)
AIC(MLexamp)
# This results in a poor model fit. 
# Let's look at a simple varying intercept model now.

# 3. Fit a varying intercept model
MLexamp.2 <- glm(extro ~ open + agree + social + class, 
                 data=lmm.data )
display(MLexamp.2)
AIC(MLexamp.2)
anova(MLexamp, MLexamp.2, test="F")

# This is the case of fitting a separate dummy variable as 
# a predictor for each class. We can see this does not provide 
# much additional model fit. Let's see if school performs any 
# better.

MLexamp.3 <- glm(extro ~ open + agree + social + school, 
                 data=lmm.data )
display(MLexamp.3)
AIC(MLexamp.3)
anova(MLexamp, MLexamp.3, test="F")

# The school effect greatly improves our model fit. 
# However, how do we interpret these effects?

table(lmm.data$school, lmm.data$class)
# Perfectly balanced design with fifty observations in 
# each combination of class and school 

# Let's try to model each of these unique cells. To do this, 
# we fit a model and use the : operator to specify the 
# interaction between school and class.

MLexamp.4 <- glm(extro ~ open + agree + social + school:class, 
                 data=lmm.data )
display(MLexamp.4)
AIC(MLexamp.4)

# What if we want to understand both the effect of the school 
# and the effect of the class, as well as the effect of the 
# schools and classes? 

MLexamp.5 <- glm(extro ~ open + agree + social + school*class - 1, 
                 data=lmm.data )
display(MLexamp.5)
AIC(MLexamp.5)

# 4. Exploring Random Slopes

require(plyr)

modellist <- dlply(lmm.data, .(school, class), function(x) 
    glm(extro ~ open + agree + social, data=x))

display(modellist[[1]])
display(modellist[[2]])


# 5. Fit a varying intercept model with lmer
MLexamp.6 <- lmer(extro ~ open + agree + social + (1|school), 
                  data=lmm.data)
display(MLexamp.6)

# We can fit multiple group effects with multiple group effect 
# terms.
MLexamp.7 <- lmer(extro ~ open + agree + social + (1|school) + 
                      (1|class), data=lmm.data)
display(MLexamp.7)

# And finally, we can fit nested group effect terms through the 
# following syntax:
MLexamp.8 <- lmer(extro ~ open + agree + social + (1|school/class), 
                  data=lmm.data)
display(MLexamp.8)

# Here the (1|school/class) says that we want to fit a mixed effect 
# term for varying intercepts 1| by schools, and for classes that are 
# nested within schools.

# 6. Fit a varying slope model with lmer

MLexamp.9 <- lmer(extro ~ open + agree + social + (1+open|school/class), data=lmm.data)
display(MLexamp.9)