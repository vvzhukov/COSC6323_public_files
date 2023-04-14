# Created 04/23/2021
# Updated 04/14/2023
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. https://www.r-bloggers.com/2021/04/logistic-regression-r-tutorial/
# 2. https://rpubs.com/jhtrygier/logistic_regression
# 3. https://www.r-bloggers.com/2020/05/multinomial-logistic-regression-with-r/
# 4. https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/

# PLAN
# 1. Logistic regression
#   Example 1 (Full model, model adjustments, model compariosnos, plot)
#   Example 2 (more visualisations, dependence plot, feature importance)
#   Example 3 (data split, preformance)
# 2. Multinomial regression
#   Example 1 (Building a Model, Predicting & Validating)
#   Example 2 (possible issue: overfitting)



# Example 1

library(readr)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(plotmo)

# Read the dataset and convert the target variable to a factor
bank_df <- read_csv2("https://raw.githubusercontent.com/vvzhukov/COSC6323_public_files/main/Lesson11_data/bank.csv")
bank_df$y = as.factor(bank_df$y)

# Plot job occupation against the target variable
ggplot(bank_df, aes(job, fill = y)) +
  geom_bar() +
  coord_flip()

bankmodel_full <- glm(y ~ .,data=bank_df, family = 'binomial')
summary(bankmodel_full)

# Fit a reduced model by dropping the term for height
bankmodel_reduced <- glm(y ~ job + marital + campaign + poutcome,data=bank_df, family = 'binomial')
summary(bankmodel_reduced) 

# Compare full model to reduced model; what null hypothesis is implied here?
anova(bankmodel_full, bankmodel_reduced, test = "Chi")

# How do we decide what to drop?
# use add1 or drop1:
drop1(bankmodel_full, test = "Chi")
add1(bankmodel_reduced, test = "Chi")



# Example 2
# Need multiple packages to be installed?
pkgs <- c("corrplot", "faraway", "glmnet", "ggplot2", "plotmo", "pdp", "vip", "earth", "rms")
lib <- installed.packages()[, "Package"]
install.packages(setdiff(pkgs, lib))

# Load western collaborative group study set from faraway package
data(wcgs, package = "faraway")
# print first few records
head(wcgs)  
# Print general structure of object
str(wcgs)

# Extract the three columns of interest and print a summary of each
summary(wcgs[, c("chd", "height", "cigs")])

# Fit an additive logistic regression model
lr.fit <- glm(chd ~ height + cigs,family = binomial(link = "logit"), data = wcgs)

# Print a summary of the fitted model
summary(lr.fit)

# Universal wy to interpret effects in a model - plots. The benefit is that your boss doesn’t care about odds, but they probably 
# care about the effects of your variables.

# Fit a reduced model by dropping the term for height
lr.fit.reduced <- glm(chd ~ cigs, family = binomial(link = "logit"), data = wcgs)

# Compare full model to reduced model; what null hypothesis is implied here?
anova(lr.fit.reduced, lr.fit, test = "Chi")

# The plotmo package
plotmo::plotmo(lr.fit)

library(ggplot2)
library(pdp)

# Set theme for ggplot2 graphics
theme_set(theme_bw())

# These plots will be linear on the logit scale but nonlinear on the probability scale 
pdp::partial(lr.fit, pred.var = "cigs", prob = TRUE, plot = TRUE, rug = TRUE,
        plot.engine = "ggplot2") +
  ggplot2::geom_rug(data = wcgs, aes(x = cigs), alpha = 0.2, inherit.aes = FALSE)

# The above is a partial dependence plot, it was introduced by Jerome Freedman, in that paper he proposed these plots as a way of 
#interpreting gradient boosted modeling. The main difference between a partial dependence plot and one of the earlier plots is that 
#rather than holding height constant,a partial dependence plot measures the impact of cigs while taking the average value of other 
#variables into account at the given level. The charts from above are called a “poor mans” partial dependence plot, as they include 
#less information, but they’re a quick and dirty way of assessing variables.

# What’s happening on the x-axis? Rug plots - the information on the bottom includes the quantity of data at each level of the data. 
# We can make some inferences about the distribution of the variable’s data based on this, there is much more data from 0 to 50 rather 
# than above 50 for # of cigs a day, so be careful not to extrapolate based on the values for which you have less data. Just eyeball 
# this data – direction & magnitude. If I plot the pdp for every variable in the model, plot them all on the same y scale.

# here, we're plotting the predicted probability of 
# developing CHD as a function of cigs while holding height constant at its 
# median value (i.e., 70 inches)
newd <- data.frame("cigs" = 0:99, height = 70)
prob <- predict(lr.fit, newdata = newd, type = "response")
plot(newd$cigs, prob, type = "l", lty = 1, las = 1, 
     xlab = "Number of cigarettes smoked per day", 
     ylab = "Conditional probabiluty of CHD")

# This is a marginal effect plot that holds height constant at 70, then plotting what happens at the predictive probability when 
# increasing cigarettes smoked per day and plotting them. Something to keep in mind.

lr.fit.all <- glm(chd ~ ., family = binomial(link = "logit"), data = wcgs)#, maxit = 9999)
# Let's inspect the data a bit more; we'll start with a SPLOM
y <- ifelse(wcgs$chd == "yes", 1, 0)
palette("Okabe-Ito")
pairs(wcgs, col = adjustcolor(y + 1, alpha.f = 0.1))
palette("default")  # back to default color pal

# Which columns contain missing values?
sapply(wcgs, FUN = anyNA)

# Which columns contain missing values?
sapply(wcgs, FUN = function(column) mean(is.na(column)))

# Look at correlations between numeric features
num <- sapply(wcgs, FUN = is.numeric)  # identify numeric columns
(corx <- cor(wcgs[, num], use = "pairwise.complete.obs"))  # simple correlation matrix

# Visualize correlations; can be useful if you have a lot of features
corrplot::corrplot(corx, method = "square", order = "FPC", type = "lower", diag = TRUE)

# We can see some high degrees of linear association, the two blood pressure variables are more closely related, linearly, than 
# our height and weight. We could assess whether to handle these variables as having multicollinearity by using variance inflation 
# factors, but, in this case, we’re not really at that point.

# What about categorical features?
xtabs(~ behave + dibep + chd, data = wcgs)

# There are two variables that perfectly separate the heart disease from non-heart disease variables with categorical data.

# Refit model without leakage or redundant features
summary(lr.fit.all <- glm(chd ~ . - typechd - timechd - behave, family = binomial(link = "logit"), data = wcgs))

# How to measure relative influence/importance of each feature?
vip::vi(lr.fit.all)  # see ?vip::vi for details
# one of the ways is to look at the regression output (z-statistic, t-statistics, equal to the coefficient divided by the error)

# The default for vip() is to take the regression model, take the absolute value of that statistic, then indicate the direction. 
#There are specific ways with specific models to measure variable importance. The absolute value of standardized coefficients 
#doesn’t apply to trees, you could check permutation importance in scikit learn, it more generally measures importance for 
#any model. If we wanted to say “how much does a variable impact predictive accuracy in the model?” for a medium-sized 
#inexpensive model (like logistic regression) is “leave one covariant out” importance, or “loco” importance. If we leave 
#out a covariant, then measure the predictive accuracy, then recalculate the predictive accuracy, we can measure how 
#important a variable is for predictions.

# Variable selection using backward elimination with AIC
lr.fit.back <- MASS::stepAIC(lr.fit.all, direction = "backward", trace = 0)
lr.fit.back
# Here we’re assessing the model with AIC - AIC, BIC, and others will penalize your model when the additional complexity of the 
# model isn’t as valuable as the predictive accuracy you gain. It works for any generalized linear model and it’s better suited 
# than a p-value for this job than p-values. How you define whether a variable gets dropped or doesn’t changes over time.



# Example 3

mydata <- read.csv('https://raw.githubusercontent.com/finnstats/finnstats/main/binary.csv')
str(mydata)

mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

xtabs(~admit + rank, data = mydata)

# Data Partition
# create training and test datasets basis 80:20 ratio
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

mymodel <- glm(admit ~ gpa + gre + rank, data = train, family = 'binomial')
#glm indicates generalized linear model and family is binomial because admit variable has only o and 1.
summary(mymodel)
# More stars indicate more statistical significance. In this case gre and level rank 2 is not statistically 
# significant. Let’s drop gre and re-run the model because gre is not significant.

mymodel <- glm(admit ~ gpa + rank, data = train, family = 'binomial') 
summary(mymodel) 
# AIC is better


# Let’s do the prediction based on the above model
p1 <- predict(mymodel, train, type = 'response')
head(p1)

# Now you can see only 28% of chances are there the first candidate to admit the project. Similarly second 
# candidate 29%, third candidate 68% and so on..

head(train)

# Misclassification error – train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1
# Based on 208+29 = 237 correct classifications and 73+15 =88 misclassifications. Let’s calculate 
# the misclassifications error rate based on below code
missclass_train <- 1 - sum(diag(tab1))/sum(tab1)
missclass_train

# Misclassification error – test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2

missclass_test <- 1 - sum(diag(tab2))/sum(tab2)
missclass_test



# Multinomial regression
# Example 1

# Multinomial regression is used to predict the nominal target variable. In case the target variable is of ordinal type, 
# then we need to use ordinal logistic regression. In this tutorial, we will see how we can run multinomial logistic 
# regression. As part of data preparation, ensure that data is free of multicollinearity, outliers, and high influential leverage points.

library(readr)
tissue <- read_csv("https://raw.githubusercontent.com/vvzhukov/COSC6323_public_files/main/Lesson12_data/BreastTissue.csv")
# Checking the structure of adult data
str(tissue)

# Combining levels of target variable and deleting the case # as it is a unique variable.
tissue <- tissue[, -1]
tissue$Class <- as.factor(tissue$Class)
levels(tissue$Class)[levels(tissue$Class) %in% c("fad", "gla", "mas")] <- "other"
levels(tissue$Class)

# Building a Multinomial Regression Model
# We will be predicting Class of the breast tissue using Breast Tissue data from the UCI machine learning repository.
#Splitting the data using a function from dplyr package

set.seed(1234)
ind <- sample(2, nrow(tissue), replace = T, prob = c(0.7, 0.3))
train <- tissue[ind==1,]
test <- tissue[ind==2,]

# Setting the reference
# Unlike binary logistic regression in multinomial logistic regression, we need to define the reference level.
train$Class <-  relevel(train$Class, ref = "adi")

# Training the multinomial classification model
require(nnet)
# Training the multinomial model
multinom_model <- multinom(Class ~ ., data = tissue)
# Checking the model
summary(multinom_model)

# Just like binary logistic regression, we need to convert the coefficients to odds by taking the exponential of the coefficients.
exp(coef(multinom_model))
# The predicted values are saved as fitted.values in the model object. Let’s see the top 6 observations.
head(round(fitted(multinom_model), 2))
# The multinomial regression predicts the probability of a particular observation to be part of the said level. 
# This is what we are seeing in the above table. Columns represent the classification levels and rows represent the observations. 
# This means that the first six observation are classified as car.

# Predicting & Validating the model

# Predicting the values for train dataset
train$ClassPredicted <- predict(multinom_model, newdata = train, "class")
# Building classification table
tab <- table(train$Class, train$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)
# Our model accuracy has turned out to be 98.68% in the training dataset.

# Predicting the class on test dataset.

test$ClassPredicted <- predict(multinom_model, newdata = test, "class")
# Building classification table
tab <- table(test$Class, test$ClassPredicted)
tab

# We were able to achieve 100% accuracy in the test dataset and this number is very close to train, and thus we conclude that 
# the model is good and is also stable.



# Example 2

# Loading the wine data
wine <- read.csv('https://raw.githubusercontent.com/vvzhukov/COSC6323_public_files/main/Lesson12_data/wine.data.csv', header= F)
colnames(wine) <- c("Type", "Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                    "Nonflavanoids", "Proanthocyanins", 
                    "Color", "Hue", "Dilution", "Proline")

# The wine dataset contains the results of a chemical analysis of wines grown in a specific area of Italy. Three types of wine are 
# represented in the 178 samples, with the results of 13 chemical analyses recorded for each sample. 

# Checking the structure of wine dataset
str(wine)

# Loading the dplyr package
library(dplyr)

wine$Type <- as.factor(wine$Type)
# Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(wine, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- wine[-sample_id,]

# To build the multinomial model we have a couple of functions in R. However, in this example we use mutinom() function from {nnet} 
# package. Remember when we build logistic models we need to set one of the levels of the dependent variable as a baseline. We 
# achieve this by using relevel() function. In other functons or algorithms, this process is generally automated.

# Setting the basline 
train$Type <- relevel(train$Type, ref = "3")
# Once the baseline has been specified, we use multinom() function to fit the model and then use summary() function to explore 
# the beta coefficients of the model.

# Loading the nnet package
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(Type ~ Alcohol + Color -1, data = train)
# We used -1 in the formula to delete the intercept. We think that it does not make sense in the model and so we removed it.

# Checking the model
summary(multinom.fit) # The output coefficients are represented in the log of odds.

# The output of summary contains the table for coefficients and a table for standard error. Each row in the coefficient table 
# corresponds to the model equation. The first row represents the coefficients for Type 2 wine in comparison to our baseline 
# which is Type 3 wine and the second row represents the coefficients for Type 2 wine in comparison to our baseline which is Type 3 wine.

# This ratio of the probability of choosing Type 2 wine over the baseline that is Type 3 wine is referred to as relative risk 
# (often described as odds). However, the output of the model is the log of odds. To get the relative risk IE odds ratio, we need to 
# exponentiate the coefficients.

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

# The relative risk ratio for a one-unit increase in the variable color is .491 for being in Type 1 wine vs. Type 3 wine. Here a 
# value of 1 represents that there is no change. However, a value greater than 1 represents an increase and value less than 1 represents a decrease.

# We can also use probabilities to understand our model.
head(probability.table <- fitted(multinom.fit))

# The table above indicates that the probability of 89th obs being Type 2 wine is 90.0%, it being Type 1 wine is 8.9%and it being Type 3 
# wine is 0.0%. Thus we can conclude that the 89th observation is Type 2. On a similar note – 57th observation is Type 1, 170th observations 
# isType 3 and so on.

# We will now check the model accuracy by building classification table. So let us first build the classification table for training dataset 
# and calculate the model accuracy.

# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")

# Building classification table
ctable <- table(train$Type, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# Accuracy in training dataset. We now repeat the above on the unseen dataset that tests dataset.

# Predicting the values for train dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable <- table(test$Type, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# Is there a problem???
# Overfitting!
# The accuracy of the test dataset turns out to be less as compared to training dataset. So we have a 
# problem of overfitting here. Despite what accuracy we get the process of building the multinomial 
# logistic regression remains the same.






# EXTRA MATERIAL
# BLOCK DESIGN
# Ref.: 
# 1. https://www.geeksforgeeks.org/completely-randomized-design-with-r-programming/
# 2. https://stat.ethz.ch/~meier/teaching/anova/block-designs.html
# 3. Mangiafico, S.S. 2016. Summary and Analysis of Extension Program Evaluation in R, version 1.18.8. rcompanion.org/handbook/. (Pdf version: rcompanion.org/documents/RHandbookProgramEvaluation.pdf.)
#       https://rcompanion.org/handbook/I_06.html
#       https://rcompanion.org/handbook/I_07.html
#       https://rcompanion.org/handbook/I_09.html

# PLAN:
# COMPLETELY RANDOMIZED DESIGN (CRD): EX1, EX2          [SKIP, for personal review]
# BLOCK DESIGN 
# RANDOMIZED COMPLETE BLOCK DESIGNS(RCBD): EX1
# LATIN SQUARE DESIGN (LSD): GEN. EX
# BLOCKS One-way ANOVA
#       1. Define linear model
#       2. Conduct analysis of variance
#       3. Histogram and plot of residuals
#       4. Post-hoc analysis: mean separation tests
# RANDOM BLOCKS One-way ANOVA
#       1. Mixed-effects model with LMER
#       2. p-value and pseudo R-squared for LMER model
#       3. Post-hoc analysis LMER
#       4. Mixed-effects model with NLME
#       5. p-value and pseudo R-squared for NLME model
#       6. Post-hoc analyzis NLME
# EXERCISE REVIEW
# REPEATED MEASURES ANOVA                               [If we have time]
#       1. Define model and conduct analysis of deviance
#       2. Test the random effects in the model
#       3. p-value and pseudo R-squared for model
#       4. Post-hoc analysis
#       5. Interaction plot
#       6. Histogram of residuals





# COMPLETELY RANDOMIZED DESIGN (CRD)

# In this type of design, blocking is not a part of the algorithm. The samples of the 
# experiment are random with replications are assigned to different experimental units.

# EXAMPLE 1
# Adding more baking powder to cakes increases the heights of the cake. 
# Let’s see with CRD how the experiment will be analyzed. 
# 1/4th table spoon to A,B,C,D
# 1/2   table spoon to B,A,C,D
# 3/4th table spoon to B,A,D,C
# 1     table spoon to A,B,D,C

# baking powder is divided into 4 different tablespoons(tbsp) and four replicate cakes 
# heights (respectively for A, B, C, D)  were made with each tbsp in random order. 
# Then the results of tbsp are compared to see if actually the height is affected by baking powder. 
# The replications are just permutations of the different cakes heights respectively for A, B, C, D. 

treat <- rep(c("A", "B", "C", "D"), each = 4)
fac <- factor(rep(c(0.25, 0.5, 0.75, 1), each = 4))

# Creating dataframe
height <- c(1.4, 2.0, 2.3, 2.5,
            7.8, 9.2, 6.8, 6.0, 
            7.6, 7.0, 7.3, 5.5,
            1.6, 3.4, 3.0, 3.9)

exp <- data.frame(treat, treatment = fac, response = height)
mod <- aov(response ~ treatment, data = exp)
summary(mod)
# p < 0.05 thus hypothesis is rejected.

# EXAMPLE 2
# Adding rocks to water increases the height of water in the container. 

# Consider that if adding four rocks to 500ml,600ml  and 700ml respectively increases 
# the height of water correspondingly. 

# 4 rocks to [500 600 700]
# 6 rocks to [600 500 700]
# 8 rocks to [700 600 500]

rocks<- rep(c("four", "six", "eight"), each = 3)
rocks
fac <- factor(rep(c(500, 600, 700), each = 3))
fac

height <- c(5, 5.5, 4.8,
            5.3, 5, 4.3,
            4.8, 4.3, 3.4)
exp1 <- data.frame(rocks, treatment = fac, 
                   response = height)
mod <- aov(response ~ treatment, data = exp1)
summary(mod)
# p > 0.05 thus hypothesis is accepted.




# BLOCK DESIGN
# RANDOMIZED COMPLETE BLOCK DESIGNS(RCBD)

# Model: Y_ij = mu + alpha_i + beta_j + e_ij
# alpha_i - treatment effects
# beta_j - block effects with the usual side-constraints
# e_ij - error term
# In this model we implicitly assume that blocks only cause additive shifts

# Create data (skip if not interested)
tip    <- factor(rep(1:4, each = 4))
coupon <- factor(rep(1:4, times = 4))
y <- c(9.3, 9.4, 9.6, 10,
       9.4, 9.3, 9.8, 9.9,
       9.2, 9.4, 9.5, 9.7,
       9.7, 9.6, 10, 10.2)
hardness <- data.frame(y, tip, coupon)

# Analyze data
fit <- aov(y ~ coupon + tip, data = hardness)                 
summary(fit)




# LATIN SQUARE DESIGN (LSD)
# special case of a multiple block factors
# so-called row-column designs

# Example of a Latin Square Design
library(agricolae)
design.lsd(LETTERS[1:4])$sketch

# A Latin Square blocks on both rows and columns simultaneously. We can use the model
# Y_ijk = mu + alpha_i + beta_j + gamma_k + e_ijk
# alpha_i - the treatment effects
# beta_j, gamma_k - block effects with side-constratints




# BLOCKS One-way ANOVA
# Blocks are used to account for suspected variation from factors other than the treatments or 
# main independent variables being investigated

# Example - sodium intake with different Instructors, use town as a blocking variable.
# may have some effect on sodium intake since each town has varying income, ethnic makeup, and 
# other demographic factors.

# The instructors are focused on the effect of their different nutrition education programs.  
# They are not concerned about sodium intake in one specific town or another per se, but they want to 
# take account this effect into account statistically.

Input = ("
Instructor        Town             Sodium
'Brendon Small'   Squiggleville    1200
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1350
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1400
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Squiggleville    1300
'Brendon Small'   Metalocalypse    1325
'Brendon Small'   Metalocalypse    1425
'Brendon Small'   Squiggleville    1500
'Brendon Small'   Squiggleville    1250
'Brendon Small'   Metalocalypse    1150
'Brendon Small'   Metalocalypse     950
'Brendon Small'   Squiggleville    1150
'Brendon Small'   Metalocalypse    1600
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Metalocalypse    1050
'Brendon Small'   Metalocalypse    1300
'Brendon Small'   Squiggleville    1700
'Brendon Small'   Squiggleville    1300
'Coach McGuirk'   Squiggleville    1100
'Coach McGuirk'   Squiggleville    1200
'Coach McGuirk'   Squiggleville    1250
'Coach McGuirk'   Metalocalypse    1050
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Metalocalypse    1250
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1350
'Coach McGuirk'   Squiggleville    1325
'Coach McGuirk'   Squiggleville    1525
'Coach McGuirk'   Squiggleville    1225
'Coach McGuirk'   Squiggleville    1125
'Coach McGuirk'   Metalocalypse    1000
'Coach McGuirk'   Metalocalypse    1125
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Metalocalypse    1200
'Coach McGuirk'   Squiggleville    1150
'Coach McGuirk'   Squiggleville    1400
'Coach McGuirk'   Squiggleville    1500
'Coach McGuirk'   Squiggleville    1200
'Melissa Robins'  Metalocalypse     900
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse    1150
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1250
'Melissa Robins'  Squiggleville    1225
'Melissa Robins'  Squiggleville    1325
'Melissa Robins'  Metalocalypse    1125
'Melissa Robins'  Metalocalypse    1025
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse     925
'Melissa Robins'  Squiggleville    1200
'Melissa Robins'  Metalocalypse    1100
'Melissa Robins'  Metalocalypse     950
'Melissa Robins'  Metalocalypse    1300
'Melissa Robins'  Squiggleville    1400
'Melissa Robins'  Metalocalypse    1100
")

Data = read.table(textConnection(Input),header=TRUE)

#  Order factors by the order in data frame
#  Otherwise, R will alphabetize them
Data$Instructor = factor(Data$Instructor,
                         levels=unique(Data$Instructor))
Data$Town       = factor(Data$Town,
                         levels=unique(Data$Town))

#  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

# Remove unnecessary objects
rm(Input)


# We can use the n columns in the Summarize output to check if cell sizes are balanced.
library(FSA)
?Summarize
Summarize(Sodium ~ Instructor + Town,
          data=Data,
          digits=3)
# The instructors have different numbers of students from each town.  That is, the design is unbalanced.

# 1. Define linear model
model = lm(Sodium ~ Instructor + Town,
           data = Data)
summary(model)

# 2. Conduct analysis of variance
library(car)
Anova(model)   # Type II sum of squares

# 3. Histogram and plot of residuals
x = residuals(model)
library(rcompanion)
plotNormalHistogram(x)

plot(fitted(model),
     residuals(model))
# (1) they’re pretty symmetrically distributed, tending to cluster towards the middle of the plot.
# (2) they’re clustered around the lower single digits of the y-axis.
# (3) in general, there aren’t any clear patterns.

# 4. Post-hoc analysis:  mean separation tests
library(multcompView)
library(lsmeans)
?lsmeans
marginal = lsmeans(model, 
                   ~ Instructor)
pairs(marginal,
      adjust="tukey")
# Calling the marginal object to be printed produces two sections of output:  $lsmeans, which 
# shows estimates for the LS means along with their standard errors and confidence intervals;  
# and $contrasts, which indicate the pairwise comparisons with p-values for the compared LS means being different.

CLD = multcomp::cld(marginal,
          alpha   = 0.05,
          Letters = letters,  # Use lower-case letters for .group
          adjust  = "tukey")  # Tukey-adjusted p-values
CLD
# Using the cld function produces a compact letter display.  LS means sharing a letter are not significantly 
# different from one another at the alpha level indicated.  A Tukey adjustment is made for multiple comparisons 
# with the adjust="tukey" option.  And Letters indicates the symbols to use for groups. 

# You may adjust the CIs for the LS means, use e.g. summary(marginal, level=0.99).

# 5. Plot of means, confidence intervals, and mean-separation letters
# Order the levels for printing
CLD$Instructor = factor(CLD$Instructor,
                        levels=c("Brendon Small",
                                 "Coach McGuirk",
                                 "Melissa Robins"))

#  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)

# Plot
library(ggplot2)
ggplot(CLD,
       aes(x     = Instructor,
           y     = lsmean,
           label = .group)) +
    
    geom_point(shape  = 15,
               size   = 4) +
    
    geom_errorbar(aes(ymin  =  lower.CL,
                      ymax  =  upper.CL),
                  width =  0.2,
                  size  =  0.7) +
    
    theme_bw() +
    theme(axis.title   = element_text(face = "bold"),
          axis.text    = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0)) +
    
    ylab("Least square mean\nsodium intake (mg / day)") +
    
    geom_text(nudge_x = c(0, 0, 0),
              nudge_y = c(120, 120, 120),
              color   = "black")

# Daily intake of sodium from diaries for students in Supplemental Nutrition Assistance Program 
# Education (SNAP-ed) workshops.  Boxes represent least square mean values for each of three classes, 
# following one-way analysis of variance (ANOVA).  Error bars indicate 95% confidence intervals of the LS means.  
# Means sharing a letter are not significantly different (alpha = 0.05, Tukey-adjusted).




# RANDOM BLOCKS One-way ANOVA
# Here, the analysis is done with a mixed effects model, with the treatments treated as a fixed effect and 
# the blocks treated as a random effect.
# We will use the same data as in BLOCKS One-way ANOVA

headTail(Data)
str(Data)
summary(Data)

# 1. Mixed-effects model with lmer
# The term (1|Town) in the model formula indicates that Town should be treated as a random variable, 
# with each level having its own intercept in the model.

library(lme4)
library(lmerTest)
model = lmer(Sodium ~ Instructor + (1|Town),
             data=Data,
             REML=TRUE)
anova(model) # The anova function in the package lmerTest is used to produce p-values for the fixed effects.
rand(model) # The rand function in the package lmerTest produces p-values for the random effects. 


# 2. p-value and pseudo R-squared for LMER model
# We need the null model with which to compare the lmer model has to be specified explicitly

# Approach 1. We may define the null model as one with no fixed effects except for an intercept, 
# indicated with a 1 on the right side of the ~.  And to also include the random effects, in this case (1|Town).
model.null = lmer(Sodium ~ 1 + (1|Town),
                  data = Data,
                  REML = TRUE)
anova(model, 
      model.null)
library(rcompanion)
?nagelkerke
nagelkerke(fit  = model,
           null = model.null)

# Approach 2. 
# To compare the model to a null model with only an intercept and neither the fixed nor the random effects.
model.null.2 = lm(Sodium ~ 1,
                  data = Data)
anova(model, 
      model.null.2)
library(rcompanion)
nagelkerke(fit  = model,
           null = model.null.2)

# 3. Post-hoc analysis
library(multcompView)
library(lsmeans)
marginal = lsmeans(model, 
                   ~ Instructor)
CLD = multcomp::cld(marginal,
          alpha=0.05, 
          Letters=letters,        # Use lower-case letters for .group
          adjust="tukey")         #  Tukey-adjusted comparisons
CLD
pairs(marginal,
      adjust="tukey")

# A similar analysis can be conducted with the difflsmeans function in the lmerTest package.
library(lmerTest)
difflsmeans(model,
            test.effs="Instructor")

# Extract a data frame from the difflsmeans output, and then add a column of p-values adjusted by the fdr method.
comparison = difflsmeans(model, 
                         test.effs="Instructor")
p.value = comparison$`Pr(>|t|)`
comparison$p.adj = p.adjust(p.value, 
                            method = "fdr")
comparison

# 4. Mixed-effects model with nlme
library(nlme)
model = lme(Sodium ~ Instructor, random=~1|Town,
            data=Data,
            method="REML")
library(car)
Anova(model)

# The random effects in the model can be tested by specifying a null model with only fixed effects 
# and comparing it to the full model with anova.
model.null = gls(Sodium ~ Instructor,
                 data=Data,
                 method="REML")
anova(model,
      model.null)

# 5. p-value and pseudo R-squared for NLME model
library(rcompanion)
nagelkerke(model)

# 6. Post-hoc analyzis
library(multcompView)
library(lsmeans)
marginal = lsmeans(model, 
                   ~ Instructor)
CLD = multcomp::cld(marginal,
          alpha   = 0.05, 
          Letters = letters,     # Use lower-case letters for .group
          adjust  = "tukey")     #  Tukey-adjusted comparisons
CLD

pairs(marginal,
      adjust="tukey")




# REPEATED MEASURES ANOVA example
# Students were asked to document their daily caloric intake once a month for six months.  
# Students were divided into three groups with each receiving instruction in nutrition education 
# using one of three curricula.

Input <- ("
Instruction        Student  Month   Calories.per.day
'Curriculum A'     a        1       2000
'Curriculum A'     a        2       1978
'Curriculum A'     a        3       1962
'Curriculum A'     a        4       1873
'Curriculum A'     a        5       1782
'Curriculum A'     a        6       1737
'Curriculum A'     b        1       1900
'Curriculum A'     b        2       1826
'Curriculum A'     b        3       1782
'Curriculum A'     b        4       1718
'Curriculum A'     b        5       1639
'Curriculum A'     b        6       1644
'Curriculum A'     c        1       2100
'Curriculum A'     c        2       2067
'Curriculum A'     c        3       2065
'Curriculum A'     c        4       2015
'Curriculum A'     c        5       1994
'Curriculum A'     c        6       1919
'Curriculum A'     d        1       2000
'Curriculum A'     d        2       1981
'Curriculum A'     d        3       1987
'Curriculum A'     d        4       2016
'Curriculum A'     d        5       2010
'Curriculum A'     d        6       1946
'Curriculum B'     e        1       2100
'Curriculum B'     e        2       2004
'Curriculum B'     e        3       2027
'Curriculum B'     e        4       2109
'Curriculum B'     e        5       2197
'Curriculum B'     e        6       2294
'Curriculum B'     f        1       2000
'Curriculum B'     f        2       2011
'Curriculum B'     f        3       2089
'Curriculum B'     f        4       2124
'Curriculum B'     f        5       2199
'Curriculum B'     f        6       2234
'Curriculum B'     g        1       2000
'Curriculum B'     g        2       2074
'Curriculum B'     g        3       2141
'Curriculum B'     g        4       2199
'Curriculum B'     g        5       2265
'Curriculum B'     g        6       2254
'Curriculum B'     h        1       2000
'Curriculum B'     h        2       1970
'Curriculum B'     h        3       1951
'Curriculum B'     h        4       1981
'Curriculum B'     h        5       1987
'Curriculum B'     h        6       1969
'Curriculum C'     i        1       1950
'Curriculum C'     i        2       2007
'Curriculum C'     i        3       1978
'Curriculum C'     i        4       1965
'Curriculum C'     i        5       1984
'Curriculum C'     i        6       2020
'Curriculum C'     j        1       2000
'Curriculum C'     j        2       2029
'Curriculum C'     j        3       2033
'Curriculum C'     j        4       2050
'Curriculum C'     j        5       2001
'Curriculum C'     j        6       1988
'Curriculum C'     k        1       2000
'Curriculum C'     k        2       1976
'Curriculum C'     k        3       2025
'Curriculum C'     k        4       2047
'Curriculum C'     k        5       2033
'Curriculum C'     k        6       1984
'Curriculum C'     l        1       2000
'Curriculum C'     l        2       2020
'Curriculum C'     l        3       2009
'Curriculum C'     l        4       2017
'Curriculum C'     l        5       1989
'Curriculum C'     l        6       2020
")
Data <- read.table(textConnection(Input),header=TRUE)

# We will use repeated measures analysis with all the measurements, treating Student as a 
# random variable to take into account native differences among students

#  Order factors by the order in data frame
#  Otherwise, R will alphabetize them

Data$Instruction = factor(Data$Instruction,
                          levels=unique(Data$Instruction))

#  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

# Remove unnecessary objects
rm(Input)

# 1. Define model and conduct analysis of deviance
library(nlme)
model = lme(Calories.per.day ~ Instruction + Month + Instruction*Month, 
            random = ~1|Student,
            correlation = corAR1(form = ~ Month | Student,
                                 value = 0.4287),
            data=Data,
            method="REML")
# corAR1 is used to indicate a temporal autocorrelation structure of order one, often abbreviated as AR(1)
# This statement takes the form:
# correlation = Structure(form  = ~ TIME | SUBJVAR)

# where:
# •  Structure is the autocorrelation structure.  Options are listed in library(nlme); ?corClasses
# •  TIME is the variable indicating time.  In this case, Month.  
#    For the corAR1 structure, the time variable must be an integer variable.
# •  SUBJVAR indicates the variable for experimental units, in this case Student.  
#    Autocorrelation is modeled within levels of the SUBJVAR, and not between them.

library(car)
Anova(model)


# 2. Test the random effects in the model
# Comparing the model to a model fitted with just the fixed effects and excluding the random effects.
model.fixed = gls(Calories.per.day ~ Instruction + Month + Instruction*Month,
                  data=Data,
                  method="REML")

anova(model,
      model.fixed)

# 3. p-value and pseudo R-squared for model
# Null model = no fixed effects except for an intercept, indicated with a 1 on the right 
# side of the ~. And to also include the random effects, in this case 1|Student.

library(rcompanion)
model.null = lme(Calories.per.day ~ 1,
                 random = ~1|Student,
                 data = Data)

nagelkerke(model, 
           model.null)
# Another approach is to compare the model to a null model with only an intercept and neither 
# the fixed nor the random effects.

library(rcompanion)
model.null.2 = gls(Calories.per.day ~ 1,
                   data = Data)

nagelkerke(model, 
           model.null.2)

# 4. Post-hoc analysis

library(multcompView)
library(lsmeans)

marginal = lsmeans(model, 
                   ~ Instruction:Month)

multcomp::cld(marginal,
    alpha   = 0.05, 
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

# 5. Interaction plot
# We will use the groupwiseMean function to calculate the natural mean of each Instruction x Month 
# combination, along with the confidence interval of each mean with the percentile method.

library(rcompanion)
Sum = groupwiseMean(Calories.per.day ~ Instruction + Month,
                    data   = Data,
                    conf   = 0.95,
                    digits = 3,
                    traditional = FALSE,
                    percentile  = TRUE)
Sum

library(ggplot2)
pd = position_dodge(.2)
ggplot(Sum, aes(x =    Month,
                y =    Mean,
                color = Instruction)) +
    geom_errorbar(aes(ymin=Percentile.lower,
                      ymax=Percentile.upper),
                  width=.2, size=0.7, position=pd) +
    geom_point(shape=15, size=4, position=pd) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    ylab("Mean calories per day")

# 6. Histogram of residuals
# Residuals from a mixed model fit with nlme should be normally distributed.

x = residuals(model)
library(rcompanion)
plotNormalHistogram(x)

# Plotting residuals vs. fitted values, to check for homoscedasticity and independence, is probably also advisable.
plot(fitted(model), 
     residuals(model))
