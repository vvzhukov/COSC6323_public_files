# 04/01/2021
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/Benchmarks/LinearMixedModels_JDS_Dec2010.pdf
# https://stats.stackexchange.com/questions/94010/understanding-dummy-manual-or-automated-variable-creation-in-glm
# https://www.jaredknowles.com/journal/2014/5/17/mixed-effects-tutorial-2-fun-with-mermod-objects
# https://rpubs.com/mlmcternan/BC-lme
# http://jakewestfall.org/misc/Winter2014.pdf

# PLAN
# 0. Dummy variables
# 1. Fit the Non-Multilevel Models
# 2. GLM
# 3. Fit a varying intercept model
# 4. Exploring Random Slopes
# 5. Fit a varying intercept model with lmer
# 6. Fit a varying slope model with lmer
# 7. Review exercise

# 0. Dummy variables
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
lmm.data <- read.table("/Users/apple/Desktop/6323_TA/R_scripts/Lesson9_data/lmm.data.txt",
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