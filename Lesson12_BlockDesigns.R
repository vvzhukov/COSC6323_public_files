# 04/23/2021
# Vitalii Zhukov
# COSC 6323
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
