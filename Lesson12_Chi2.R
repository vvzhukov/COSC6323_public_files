# Created 04/22/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# 1. http://www.r-tutor.com/elementary-statistics/goodness-fit/multinomial-goodness-fit
# 2. https://www.stat.berkeley.edu/~stark/SticiGui/Text/chiSquare.htm
# 3. https://cran.r-project.org/web/packages/XNomial/vignettes/XNomial.html
# 4. https://www.datacamp.com/community/tutorials/contingency-tables-r
# 5. https://data.library.virginia.edu/an-introduction-to-loglinear-models/
# 6. Agresti, A. An Introduction to Categorical Data Analysis, 1st Ed. 1996. Ch. 6.
# 7. Faraway, J. Extending the Linear Model with R. 2006. Ch. 4.
# 8. Venables, W.N and Ripley, B.D. Modern Applied Statistics with S, 4th Ed. 2002. Ch. 7.

# PLAN
# 1. Hypothesis test for a multinomial population
#       Goodness of fit using the chi^2 test
# 2. Contingency tables
# 3. Loglinear model


# 1. Hypothesis test for a multinomial population
# When do we call population multinomial?
# If its data is categorical and belongs to a collection 
# of discrete non-overlapping classes

# H0:  goodness of fit test for multinomial distribution 
# is that the observed frequency 'fi' is equal to 
# an expected count 'ei' in each category. 
# Rejected if p < alpha

# Example
# We will use data from the MASS package
library(MASS)

?survey
# Take a look at the data
head(survey)
# Check for the factors levels
levels(survey$Smoke)
# Build frequency table
table(survey$Smoke)
smoke.freq = table(survey$Smoke)

# Problem definition:
# Suppose the campus smoking statistics is as below. 
#   Heavy   Never   Occas   Regul 
#   4.5%   79.5%    8.5%    7.5%
# Determine whether the sample data in survey 
# supports it at .05 significance level.

smoke.prob = c(0.045, 0.795, 0.085, 0.075)
?chisq.test
chisq.test(smoke.freq, p=smoke.prob)
# Here p = 0.99 > alpha (0.05) so H0 stands and
# the sample data in survey supports the campus-wide 
# smoking statistics

# Lets review another useful package for 
# multinomial problems
library(XNomial)
# Two main functions: xmulti() and xmonte()
# Both used to finding the P value used to test the 
# goodness-of-fit for a multinomial distribution with 
# fixed probabilities

# With xmulti(), all possible outcomes will be examined 
# for an exact test (preferable, except when it takes too long)

# With xmonte(), a random set of possible outcomes is used 
# for a monte-carlo test

# Three approaches used inside:
# 1. The likelihood ratio(LLR) 
# This is the ratio of the 
# probability of the observed result under the null 
# hypothesis over its probability given the alternative. 
# 2. The multinomial probability itself
# This is simply the probability of a given outcome 
# under the null
# 3. The classic chi-squared statistic (Pearson’s Chi^2)


# Example. Gregor Mendel’s data
# The Austrian monk performed some crosses with garden pea plants.

# The four categories were round/yellow, round/green, 
# wrinkled/yellow and wrinkled/green, and his 
# counts were 315, 108, 101 and 32 respectively.

peasF2 <- c(315, 108, 101, 32)
# According to his genetics model the ration should be
peasExp <- c(9, 3, 3, 1)

# Lets test if his model was solid:
xmulti(peasF2, peasExp)
# Very high P value. 
# The tendency for Mendel’s results to fit too well has not 
# escaped the attention of historians!

# More details:
xmulti(peasF2, peasExp, detail=3)
# It also reports the total amount of combinations
# xmulti has to look at (total number of tables)

# We can also xmulti to plot histogram and see the shape of LLR
# distribution
xmulti(peasF2, peasExp, histobins=T)
# The blue curve shows that the asymptotic Chi^2
# distribution is not a bad fit in this case.

# Now imagine that Mendel looked closer at his 556 seeds 
# and noticed that the ones he had classified as yellow 
# actually fell into two categories: 
# light-yellow and dark-yellow. 
# So now, instead of four kinds of seeds, he had six: 
# round/dark-yellow
# round/light-yellow
# round/green
# wrinkled/dark-yellow
# wrinkled/light-yellow
# wrinkled/green

# and reclassified numbers were:
rePeas <- c(230, 85, 108, 80, 21, 32)
# If we assume that the two shades of yellow represent peas 
# that were homozygous (light-yellow) or 
# heterozygous (dark-yellow) for the yellow allele, 
# then from standard genetics we expect the ratios 
# to be 6:3:3:2:1:1.
reExp <- c(6,3,3,2,1,1)

xmulti(rePeas, reExp)
# We got a warning: too many tables to review!

xmonte(rePeas, reExp)
xmonte(rePeas, reExp, detail=3, histobins=T)
# In this case, P is much smaller than in Mendel’s 
# real data, indicating that the fit is not nearly 
# so good, and we will want to modify our hypothesis.

# Perhaps the two shades of yellow were too 
# close for the human eye to distinguish accurately 
# and misclassifications occurred.


# 2. Contingency tables
# a table showing the distribution of one variable in rows 
# and another in columns, used to study the association 
# between two variables.

library(MASS)
?Cars93
head(Cars93)

# We have 6 types of cars
levels(Cars93$Type)
table(Cars93$Type)
?prop.table()

# Function prop.table converts it into fractions:
prop.table(table(Cars93$Type))*100

# Same with origin
table(Cars93$Origin)
prop.table(table(Cars93$Origin))

# Now Let's look at types of cars with respect to their origin
CarType_Origin <- table(Cars93$Type, Cars93$Origin)
CarType_Origin
# We can get marginal using rowSums and colSums
rowSums(CarType_Origin)
colSums(CarType_Origin)

# Probabilities
prop.table(CarType_Origin)*100

# We can use margin argument to check 
# the distribution within group
prop.table(CarType_Origin, margin = 2)*100

# We may apply Chi^2 test here as well
# Lets see check Type and Origin are independent:
chisq.test(Cars93$Type, Cars93$Origin)
# Apparently, they're not.
# Got warning chi-squared statistic follows chi-squared 
# distribution only approximately. The more observations 
# we have, the better approximation is.

# Function chisq.test function throws the above warning whenever 
# one of the expected counts is lower than 5


# 3. Loglinear model

# Create data
seniors <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), 
                 dim = c(2,2,2), 
                 dimnames = list("cigarette" = c("yes","no"),
                                 "marijuana" = c("yes","no"),
                                 "alcohol" = c("yes","no")))

# Here we used array to create a table with more 
# than 2 dimensions
seniors

# Total of 2276 have participated in the survey
# At first we may want to check margins:
addmargins(seniors)

# 1 means rows, 2 means columns, and 3 and 
# above refer to the layers. Below we calculate 
# proportions across the columns along the 
# rows for each layer. Hence the argument margin = c(1,3)
prop.table(seniors, margin = c(1,3))*100

# We see that of those students who tried cigarettes 
# and alcohol, 62% also tried marijuana. 

# Likewise, of those students who did not try cigarettes 
# or alcohol, 99% also did not try marijuana. 
# Some sort of relationship here?!

# In order to use glm() we need to transform the data
seniors.df <- as.data.frame(as.table(seniors))
seniors.df[,-4] <- lapply(seniors.df[,-4], relevel, ref = "no")
seniors.df

# Let’s work with our survey data, which is a three-way 
# contingency table. To start, we model Freq as a 
# function of the three variables using the glm function. 
# Notice we set the family argument to poisson since 
# we’re modeling counts. 

mod0 <- glm(Freq ~ cigarette + marijuana + alcohol, 
            data = seniors.df, family = poisson)
summary(mod0)

# We see highly significant coefficients and p-values 
# near 0. But for loglinear models we want to check the 
# residual deviance. As a rule of thumb, we’d like it 
# to be close in value to the degrees of freedom. 
# Here we have 1286 on 4 degrees of freedom. 
# This indicates a poor fit. We can calculate a 
# p-value if we like.

# The deviance statistic has an approximate chi-squared 
# distribution, so we use the pchisq function
pchisq(deviance(mod0), df = df.residual(mod0), lower.tail = F)

# The null of this test is that the expected 
# frequencies satisfy the given loglinear model. 
# Clearly they do not

# More intuitive way to compare fitted vs observed:
cbind(mod0$data, fitted(mod0))
# Fitted is way off the observed data

# Coefficients interpretation:
exp(coef(mod0)[3])

# That’s the odds of using marijuana because “no” 
# is the baseline. We can check this manually by 
# calculating the odds directly:
margin.table(seniors, margin = 2)/sum(margin.table(seniors, margin = 2))
0.4217926 / 0.5782074

# Let’s fit a more complex model that allows variables 
# to be associated with one another, but maintains the 
# same association regardless of the level of the third 
# variable. We call this homogeneous association.

# This says that, for example, alcohol and marijuana use 
# have some sort of relationship, but that relationship 
# is the same regardless of whether or not they tried 
# cigarettes. 

# We will add “all variables and all pairwise interactions”
mod1 <- glm(Freq ~ (cigarette + marijuana + alcohol)^2, 
            data = seniors.df, family = poisson)
summary(mod1)

# Much better. Notice the residual deviance (0.37399) 
# compared to the degrees of freedom (1). We can 
# calculate a p-value if we like:

pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = F)
cbind(mod1$data, fitted(mod1))

# How the interaction can be interpreted?
exp(coef(mod1)["marijuanayes:alcoholyes"])
# Students who tried marijuana have estimated odds of 
# having tried alcohol that are 19 times the estimated 
# odds for students who did not try marijuana.
exp(coef(mod1)["cigaretteyes:alcoholyes"])

# Confidence intervals:
exp(confint(mod1, parm = c("cigaretteyes:marijuanayes",
                           "cigaretteyes:alcoholyes",
                           "marijuanayes:alcoholyes")))

# the odds of trying marijuana if you tried cigarettes is 
# at least 12 times higher than the odds of trying 
# marijuana if you hadn’t tried cigarettes, and vice versa.

# Now lets fit a model with a three-way interaction.
mod2 <- glm(Freq ~ cigarette * marijuana * alcohol, 
            data = seniors.df, family = poisson) 
# This is the saturated model since it has as many 
# coefficients as cells 

summary(mod2)
# The deviance of this model is basically 0 on 0 degrees 
# of freedom. The fitted counts match the observed counts

# It’s useful to fit a saturated model to verify the 
# higher-order interaction is statistically not significant

# We can compare fitting of mod1 vs mod2 by performing a 
# likelihood ratio test. One way to do this is with the 
# anova function

anova(mod1, mod2)
pchisq(0.374, df = 1, lower.tail = F)
# We fail to reject the null hypothesis that mod1 fits 
# just as well as mod2

# We may also try to include only specific interactions:
mod3 <- glm(Freq ~ (cigarette * marijuana) + 
                (alcohol * marijuana), 
            data = seniors.df, family = poisson)

anova(mod3, mod1)
pchisq(187.75, df = 1, lower.tail = F)

# The probability of seeing such a big change in 
# deviance (187.38) if the models really were no 
# different is remote. There appears to be good 
# evidence that the homogeneous association model 
# provides a much better fit than the model that 
# assumes conditional independence between alcohol 
# and cigarette use.

# Loglinear models work for larger tables that extend 
# into 4 or more dimensions. Hard interpretation.