# Created 04/15/2021
# Updated 04/15/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. http://www.lithoguru.com/scientist/statistics/course.html
# 2. https://towardsdatascience.com/design-of-experiments-with-r-e54167fac490

# PLAN
# EXAMPLE 1
#   Building design
#   Some data visualizations
#   LM
#   ANOVA
# EXAMPLE 2
#   Another way to build design
#   LM
#   explaining results
# EXAMPLE 3 Book example review (Polynomial model)

setwd("/Users/apple/Desktop/6323_TA/Git_code/Lesson11_data/")

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
