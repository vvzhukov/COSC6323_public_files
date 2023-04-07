# Created 04/15/2021
# Updated 04/07/2023
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# http://www.lithoguru.com/scientist/statistics/course.html
# https://towardsdatascience.com/design-of-experiments-with-r-e54167fac490
# https://www.r-tutor.com/elementary-statistics/goodness-fit/multinomial-goodness-fit
# http://www.sthda.com/english/wiki/chi-square-goodness-of-fit-test-in-r
# https://search.r-project.org/CRAN/refmans/EnvStats/html/gofTest.html
# https://rinterested.github.io/statistics/chi_square_GOF.html
# testshttps://www.r-bloggers.com/2020/12/contingency-tables-in-r/

# PLAN
# 1) Chi square test on multinomial populations
# 2) Goodness of fit test, mainly for discrete distributions
#    - Kolmogorov-Smirnov 
#    - Cramer-von Mises
#    - Anderson-Darling 
# 3) Contingency tables with homogeneity and independence 
#    - Mosaic plots
#    - Two/Three ways tables


# 1) Chi square test on multinomial populations
# Multinomial Goodness of Fit
# A population is called multinomial if its data is categorical 
# and belongs to a collection of discrete non-overlapping classes.

library(MASS) 
data(survey)
?survey
levels(survey$Smoke) 

smoke.freq <- table(survey$Smoke) 
smoke.freq 

# Problem
# Suppose the campus smoking statistics is as below. Determine 
# whether the sample data in survey supports it at .05 
# significance level.

# Heavy   Never   Occas   Regul 
# 4.5%   79.5%    8.5%    7.5%

# Create the vector representing existing proportions:
smoke.prob <- c(.045, .795, .085, .075) 
# Run Chi squared test:
chisq.test(smoke.freq, p=smoke.prob) 

# As the p-value 0.991 is greater than the .05 significance level, 
# we do not reject the null hypothesis that the sample data in 
# survey supports the campus-wide smoking statistics.


# Form the theory and example above
# The chi-square goodness of fit test is used to compare the observed 
# distribution to an expected distribution, in a situation where we 
# have two or more categories in a discrete data. In other words, 
# it compares multiple observed proportions to expected probabilities.


#    Example
# we collected wild tulips and found that 81 were red, 
# 50 were yellow and 27 were white.
#    Q1
# If these colors were equally distributed, the expected 
# proportion would be 1/3 for each of the color.
#    Q2
# Suppose that, in the region where you collected the data, 
# the ratio of red, yellow and white tulip is 3:2:1 (3+2+1 = 6). 
# This means that the expected proportion is:
# 3/6 (= 1/2) for red 
# 2/6 ( = 1/3) for yellow
# 1/6 for white

# We want to know, if there is any significant difference between 
# the observed proportions and the expected proportions.


# H0: There is no significant difference between the observed and the expected value.
# H1: There is a significant difference between the observed and the expected value.

# Q1
tulip <- c(81, 50, 27)
res <- chisq.test(tulip, p = c(1/3, 1/3, 1/3))
res

# The p-value of the test is 8.80310^{-7}, which is less than 
# the significance level alpha = 0.05. We can conclude that the 
# colors are significantly not commonly distributed with 
# a p-value = 8.80310^{-7}.

# IMPORTANT: 
# Use chisq.test only when all calculated expected values are 
# greater than 5.

# Q2
tulip <- c(81, 50, 27)
res <- chisq.test(tulip, p = c(1/2, 1/3, 1/6))
res

# The p-value of the test is 0.9037, which is greater than the 
# significance level alpha = 0.05. We can conclude that the 
# observed proportions are not significantly different from the expected proportions.

# As previously we can access data 
res$
# printing the p-value
res$p.value


# 2) Goodness of fit test, mainly for discrete distributions
# Goodness of fit
# What about cases when we want to determine if the data
# came from same types of distribution?
library(EnvStats)
# Tests:
# Kolmogorov-Smirnov 
# Cramer-von Mises
# Anderson-Darling 
?gofTest

# Example 1
# Generate 20 observations from a gamma distribution with
# parameters shape = 2 and scale = 3 then run various
# goodness-of-fit tests.
# (Note:  the call to set.seed lets you reproduce this example.)

set.seed(47)
dat <- rgamma(20, shape = 2, scale = 3)

# Shapiro-Wilk generalized goodness-of-fit test
gof.list <- gofTest(dat, distribution = "gamma")
gof.list

dev.new()
plot(gof.list)

# Komogorov-Smirnov goodness-of-fit test 
# (pre-specified parameters)

gofTest(dat, test = "ks", distribution = "gamma",
        param.list = list(shape = 2, scale = 4))

# Again here:
# H0: data came from the same distribution
# H1: data came from a different distribution


# Example
# Generate 20 observations from a normal distribution 
# with mean=3 and sd=2, and generate 10 observaions 
# from a normal distribution with mean=2 and sd=2 then
# test whether these sets of observations come from the 
# same distribution.
# (Note: the call to set.seed simply allows you to 
# reproduce this example.)

set.seed(300)
dat1 <- rnorm(20, mean = 3, sd = 2)
dat2 <- rnorm(10, mean = 1, sd = 2)
gofTest(x = dat1, y = dat2, test = "ks")


library(goftest)
x <- rnorm(30, mean=2, sd=1)
# default behaviour: 
# parameters fixed: simple null hypothesis
cvm.test(x, "pnorm", mean=2, sd=1)
ad.test(x, "pnorm", mean=2, sd=1)

# parameters estimated: 
# composite null hypothesis
mu <- mean(x)
sigma <- sd(x)
cvm.test(x, "pnorm", mean=mu, sd=sigma, estimated=TRUE)
ad.test(x, "pnorm", mean=mu, sd=sigma, estimated=TRUE)


# 3) Contingency tables with homogeneity and independence 
# Contingency Tables in R

# A common way to represent and analyze categorical data 
# is through contingency tables. 

library(ISLR)
?Wage
data(Wage)

# Two-way tables
# Two-way tables involve two categorical variables, X 
# with r categories and Y with c. Therefore, there are 
# r times c possible combinations.

library(tidyverse)
library(Rfast)
library(MASS)

# create the wage_cat variable which takes two values
# such as Above if the wage is above median and Below if
# the wage is below median
Wage$wage_cat<-
    as.factor(ifelse(Wage$wage>median(Wage$wage),
                     "Above","Below"))

# Examine the Wage vs Job Class
xtabs(~jobclass+wage_cat, data=Wage)
# or
con1<-table(Wage$jobclass,Wage$wage_cat)
con1

# The most proper way to represent graphically the 
# contingency tables are the mosaic plots:
mosaicplot(con1)

# From the mosaic plot above we can easily see that 
# in the Industrial sector the percentage of people 
# who are below the median are more compared to those 
# who work in the Information industry.

# Proportions of the Contingency Tables
# We can get the proportions of the Contingency Tables, 
# on overall and by rows and columns. Let’s see how 
# we can do it:
    
# overall
prop.table(con1)
# by row
prop.table(con1, margin = 1)
# by column
prop.table(con1, margin = 2)

# We can add the rows and columns totals of the 
# contingency tables as follows:
addmargins(con1)

# We can apply the following statistical tests in 
# order to test if the relationship of these two 
# variables is independent or not.
chisq.test(con1)
# As we can see the p-value is less than 5% thus 
# we can reject the null hypothesis that the 
# jobclass is independent to median wage.

# Fisher’s Exact Test
# When the sample size is low, we can apply the 
# Fisher’s exact test instead of Chi-Square test.
fisher.test(con1)
# Again, we see that we reject the null hypothesis.

# Log Likelihood Ratio
# Another test that we can apply is the Log 
# Likelihood Ratio using the MASS package:
?loglm
loglm( ~ 1 + 2, data = con1)
# If the left-hand side is empty, the data argument 
# is required and must be a (complete) array of 
# frequencies. In this case the variables on the 
# right-hand side may be the names of the dimnames 
# attribute of the frequency array, or may be 
# the positive integers: 1, 2, 3, ... used as 
# alternative names for the 1st, 2nd, 3rd, ... 
# dimension (classifying factor). If the left-hand 
# side is not empty it specifies a vector of 
# frequencies. In this case the data argument, if 
# present, must be a data frame from which the 
# left-hand side vector and the classifying factors 
# on the right-hand side are (preferentially) obtained. 
# The usual abbreviation of a . to stand for 
# ‘all other variables in the data frame’ is allowed. 
# Any non-factors on the right-hand side of the 
# formula are coerced to factor.

# Reject the null again

# Notice that if we run the same analysis by comparing 
# the wage median versus race and the wage median 
# vs education, we find tha there is a statistical 
# significance difference in both cases.

con2<-table(Wage$education,Wage$wage_cat)
con2
mosaicplot(con2)
chisq.test(con2)

con3<-table(Wage$race,Wage$wage_cat)
con3
mosaicplot(con3)
chisq.test(con3)


# Three-Way Tables

# Let’s say that now we want to create contingency 
# tables of three dimensions such as wage median, 
# race and jobclass
con4<-xtabs(~jobclass+wage_cat+race, data=Wage)
ftable(con4)

# change the share of 
# the rows and columns.
con4 %>% ftable(row.vars=c("race", "jobclass"))

# get the probabilities by row
con4 %>% ftable(row.vars=c("race", "jobclass")) %>%
    prop.table(margin = 1) %>%
    round(2)

# At the certain number of the factors
# it is harder to visualize the data
mosaicplot(con4)


# Cochran-Mantel-Haenszel (CMH) Methods
# We are dealing with a 2x2x4 table where the race 
# has 4 levels. We want to test for conditional 
# independence and homogeneous associations 
# with the K conditional odds ratios in 2x2x4 table. 

# With the CMH Methods, we can combine the sample 
# odds ratios from the 4 partial tables into a 
# single summary measure of partial association. 
# In our case, we have the wage_cat (Above, Below) 
# the jobclass (Industrial, Information) and the 
# race (White, Black, Asian, Other). We want to 
# investigate the association between wage_cat and 
# jobclass while controlling for race.

# H0: wage_cat and jobclass are conditionally 
# independent, given the race, which means that 
# the odds ratio of wage_cat and jobckass is 1 for 
# all races versus at least one odds ratio is not 1.

# get the 4 odds ratio
library(Rfast)
for (i in 1:4) {
    print(odds.ratio(con4[,,i])$res[1])
}
# As we can see, the odds ratios are not close to 1, 
# so we expect to reject the null hypothesis. Let’s 
# run the CHM test:

# CMH Test
mantelhaen.test(con4)
# As expected, we rejected the null hypothesis since the p-value 
# is less than 5%.

# EXTRA MATERIAL

setwd("Lesson11_data")

# An experiment that has each combination of all factor levels 
# applied to the experimental 
# units is called a factorial experiment.

# EXAMPLE 1

# First, some tools for creating designs

# install.packages("AlgDesign")
library(AlgDesign)

# As an example, we'll create full factorial design with 
# three factors (F1, F2, F3) and with 3, 2, and 3 levels, 
# respectively

gen.factorial(levels = c(3,2,3), 
              nVars = 3, 
              center=TRUE, 
              varNames=c("F1", "F2", "F3"))
# When center = TRUE, the design is symmetric and the three levels will be 
# signified by -1, 0, and 1

# The common 2^k design (two levels for each factor):
gen.factorial(levels = 2, 
              nVars = 3, 
              varNames=c("F1", "F2", "F3"))

# the output is a data frame containing the factorial design

# Factorial Design example from the book, 
# Box, Hunter, and Hunter, Statistics for Experimenters, John Wiley & Sons (1978).
yield <- read.csv('FactorialDesign.csv')[-1]

# Note that this is a two-level, three factor, full factorial 
# design with Yield as the response (no replicates)

# There are data for two catalysts (0, 1); let's convert it 
# into a factor
yield$Catalyst = factor(yield$Catalyst)
yield$Conc = factor(yield$Conc)
yield$Temp = factor(yield$Temp)
# A few plots can help us see the data
plot(yield)

par(mfrow=c(2,2))
interaction.plot(yield$Temp,yield$Conc,yield$Yield)
interaction.plot(yield$Temp,yield$Catalyst,yield$Yield)
interaction.plot(yield$Conc,yield$Catalyst,yield$Yield) 
par(mfrow=c(1,1))

# install.packages("lattice")
library(lattice)

xyplot(Yield ~ Temp | Catalyst, data = yield,
       panel = function(x, y, ...)
       {
           panel.xyplot(x, y, ...)
           panel.lmline(x, y, ...)
       }
)

xyplot(Yield ~ Conc | Catalyst, data = yield,
       panel = function(x, y, ...)
       {
           panel.xyplot(x, y, ...)
           panel.lmline(x, y, ...)
       }
)

par(mfrow=c(2,2))
boxplot(Yield~Temp, data=yield, main="Yield by Temperature",
        xlab="Temperature (C)",ylab="Percent Yield")

boxplot(Yield~Conc, data=yield, main="Yield by Concentration",
        xlab="Concentraion (wt%)",ylab="Percent Yield")

boxplot(Yield~Catalyst, data=yield, main="Yield by Catalyst",
        xlab="Catalyst Type",ylab="Percent Yield")
par(mfrow=c(1,1))

# Alternately, combine boxpolts into one graph
boxplot(Yield ~ Temp*Conc, data=yield, ylab="% Yield", xlab="Temp.Conc")
boxplot(Yield ~ Temp*Catalyst, data=yield, ylab="% Yield", xlab="Temp.Catalyst")


# LINEAR MODELS
# let's model the main effects
model1 = glm(Yield ~ ., data = yield)
# Yield ~ Temp + Conc + Catalyst
summary(model1)

# Now we'll add two-factor interactions


model2 = glm(Yield ~ .^2, data = yield)
# Same as: Yield ~ Temp*Conc + Catalyst*Temp + Catalyst*Conc
# Temp*Conc == Temp + Conc + Temp:Conc
# And Temp*Conc = Temp + Conc + Temp:Conc
summary(model2)

#compare the two models
anova(model1, model2)

# Finally, the full model including three-factor interactions
model3 = glm(Yield ~ .^3, data = yield)
summary(model3)
# Note that since we have no replicates, we have no way of assessing 
# the significance of any of the coefficients of this full model 
# with all interactions

# also note that we could model this with some, but not all 
# of the interactions present
model4 = glm(Yield ~ Conc + Temp*Catalyst, data = yield)
model5 = glm(Yield ~ Temp + Conc + Catalyst + Temp:Catalyst, data = yield)
summary(model4)
summary(model5)
# Note that Y ~ X1 * X2 is the same as Y ~ X1 + X2 + X1 : X2
# and ":" means 'interaction between'

anova(model4,model2)


# ANOVA
# Another way to model the response as a function of the factors 
# is with an anova table.  
# This requires all variables to be factors.

yield.aov = yield
yield.aov$Conc = factor(yield.aov$Conc)
yield.aov$Temp = factor(yield.aov$Temp)

aov1.out = aov(Yield ~ ., data=yield.aov)
summary(aov1.out)

aov2.out = aov(Yield ~ .^2, data=yield.aov)
summary(aov2.out)

# Number of params = number of data points
aov3.out = aov(Yield ~ .^3, data=yield.aov)
summary(aov3.out)

# Based on these results, I think a Conc + Temp*Catalyst model may be the best
aov4.out = aov(Yield ~ Conc + Temp*Catalyst, data=yield.aov)
summary(aov4.out)

# You can plot the ANOVA results in a couple of ways:
plot(aov4.out, 1)           # residuals versus fitted values
plot(aov4.out, 2)           # QQ plot

# This provides all of the means commonly calculated in 
# ANOVA analysis  (review all combinations)
model.tables(aov2.out, type="means", se=T)


# EXAMPLE 2

# Import SixSigma package
# install.packages('SixSigma')
library(SixSigma)

# Design the experiment (2^3)
ExperimentDesign <- expand.grid(A = gl(2, 1, labels = c("-", "+")),
                                B = gl(2, 1, labels = c("-", "+")),
                                C = gl(2, 1, labels = c("-", "+")))
ExperimentDesign

# Randomize the experiment
ExperimentDesign$ord <- sample(1:8, 8)
ExperimentDesign[order(ExperimentDesign$ord), ]

# Create replicates
ss.data.doe1 <- data.frame(repl = rep(1:2, each = 8),
                           rbind(ExperimentDesign))
ss.data.doe1

# Add responses
ss.data.doe1$response <- c(5.33, 6.99, 4.23, 6.61,
                           2.26, 5.75, 3.26, 6.24,
                           5.7, 7.71, 5.13, 6.76,
                           2.79, 4.57, 2.48, 6.18)
ss.data.doe1


# Get the average score for each experiment design
aggregate(response ~ A + B + C,
          FUN = mean, data = ss.data.doe1)

# Get results
doe.model <- glm(response ~ A + B + C +
                    A * B + A * C + B * C +
                    A * B * C,
                data = ss.data.doe1)
summary(doe.model)

# Effects of factors C and A are significant, the effort of 
# factor B is not.
# Interactions among the factors are neither 2-way or 3-way, 
# maknig them insignificant.
# We can now simplify the model:

# Simplify the model by excluding nonsignificant effects
doe.model.2 <- lm(response ~ A + C,
                  data = ss.data.doe1)
summary(doe.model.2)

# Obtain model's coefficients
coef(doe.model.2)

# Obtain estimations for all experimental conditions 
# (including the replications)
predict(doe.model.2)

# Compute confidence interval for each factpr
confint(doe.model.2)

# Factor A Plot
plot(c(-1,1), ylim = range(ss.data.doe1$response),
     coef(doe.model)[1] + c(-1,1) * coef(doe.model)[2],
     type = "b", pch = 16)
abline(h = coef(doe.model)[1])

# We can observe that when factor A is set on the "+" level (1.0)
# the outcome is grater than when it is set on the "-" level (-1.0)

# Visualize the main effects in separate plots
prinEF <- data.frame(Factor = rep(c("A","C"),each = 2),
                     Level = rep(c(-1,1), 2),
                     Response = c(aggregate(response ~ A, FUN = mean, data = ss.data.doe1)[,2],
                                  aggregate(response ~ C, FUN = mean, data = ss.data.doe1)[,2]))
library(ggplot2)
main_effects <- ggplot(prinEF,
                       aes(x = Level, y = Response)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = c(-1, 1)) +
    facet_grid(. ~ Factor)
main_effects

# For factor C situation is different "+" will give us 
# lower outcome

# Plot the effects of the interactions
intEf <- aggregate(response ~ A + C,
                   FUN = mean, data = ss.data.doe1)
effects_interaction <- ggplot(intEf, aes(x = A, y = response, color = C)) +
    geom_point() + 
    geom_line(aes(group = C))
effects_interaction

# Both lines do not intersect => there is no 
# interaction between these 2 factors.
# In order to max outcome: A should be '+", C - "-"

# Now lets check the residual plot
# Residuals plots standard chart
par(mfrow=c(2,2))
plot(doe.model.2)
box("outer")

# Check normality of the residuals with a normality test
shapiro.test(residuals(doe.model.2))

# Residual plot no patterns, QQ plot is not straight enough 
# (potential issues)

# EXAMPLE 3 Book example review
# Polynomial model
# "curve fitting". 
# That is, the interest is in the nature of the fitted 
# response curve rather than in 
# the partial regression coefficients.  

# This example concerns the growth of rabbit jawbones. 
# Measurements were made on lengths 
# of jawbones for rabbits of various ages.
data_e3 <- data.frame('AGE' = c(0.01,0.2, 0.2, 0.21, 0.23, 0.24, 0.24, 0.25, 0.26, 0.34, 0.41,
                             0.83, 1.09, 1.17, 1.39, 1.53, 1.74, 2.01, 2.12, 2.29, 2.52, 
                             2.61, 2.64, 2.87, 3.39, 3.41, 3.52, 3.65),
                    'LENGTH' = c(15.5, 26.1, 26.3, 26.7, 27.5, 27.0, 27.0, 26.0, 28.6, 29.8,
                                 29.7, 37.7, 41.5, 41.9, 48.9, 45.4, 48.3,50.7,50.6, 49.2,49.0,
                                 45.9,49.8,49.4,51.4,49.7,49.8,49.9))
par(mfrow=c(1,1))

# take a look at the data
plot(data_e3)

# fourth-degree polynomial model 
model_ex3 <- lm(LENGTH~poly(AGE,4), data=data_e3)
summary(model_ex3)
?poly

# now lets take a look at the type of the fitted model
ggplot(data_e3, aes(x = AGE, y = LENGTH)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue", se = F) +
    geom_smooth(method = "loess", col = "red", se = F)


# all model stats are the same as previously
summary(model_ex3)
confint(model_ex3, level=0.95)

predicted.intervals <- predict(model_ex3,data.frame(AGE=data_e3$AGE),interval='confidence',
                               level=0.99)

plot(data_e3)
lines(data_e3$AGE,predicted.intervals[,1],col='green',lwd=3)
lines(data_e3$AGE,predicted.intervals[,2],col='black',lwd=1)
lines(data_e3$AGE,predicted.intervals[,3],col='black',lwd=1)
legend("bottomright",c("Observ.","Predicted"), 
       col=c("black","green"), lwd=3)

#install.packages("sjPlot")
library(sjPlot)
plot_model(doe.model, "int")[[1]] +
    font_size(title = 15,axis_title.x = 15,axis_title.y = 15)
