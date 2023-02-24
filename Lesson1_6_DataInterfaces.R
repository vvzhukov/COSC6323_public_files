# Created: 03/04/2021
# Updated: 02/24/2023
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# https://www.statology.org/transform-data-in-r/

# Plan:
# 1. Understanding ANOVA
#       TukeyHSD, General Linear Hypotheses, 
#       Pairwise t tests, assumptions test, 
#       Kruskal-Wallis rank sum test
# 2. ANOVA examples
# 3. Homogeneous variances test
# 4. Tukey's procedure
# 5. Data transformations and when to use it?

# ----------------------------------------------------------------------------------
# Understanding ANOVA
# ----------------------------------------------------------------------------------

?PlantGrowth
my_data <- PlantGrowth

# Show a random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

# Show the levels
levels(my_data$group)

# Re-order groups
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

# Summary stats

library(dplyr)
group_by(my_data, group) %>%
    summarise(
        count = n(),
        mean = mean(weight, na.rm = TRUE),
        sd = sd(weight, na.rm = TRUE)
    )

# Visualization
# install.packages("ggpubr")

# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment, n=10 per group")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

# Compute the analysis of variance
?aov
res.aov <- aov(weight ~ group, data = my_data)

# Summary of the analysis
summary(res.aov)

# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences 
# between the groups highlighted with “*" in 
# the model summary.
 
# Significant p-value indicates that some of the group 
# means are different, but we don’t know which pairs 
# of groups are different

# As the ANOVA test is significant, we can compute Tukey HSD 
# (Tukey Honest Significant Differences)

TukeyHSD(res.aov)

# Here
# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the 
# confidence interval at 95% (default) p adj: p-value 
# after adjustment for the multiple comparisons.

# RESULT
# It can be seen from the output, that only the difference 
# between trt2 and trt1 is significant with an adjusted 
# p-value of 0.012.


# ALTERNATIVE 1
# We may also use glht() function from multcomp package 
# to perform multiple comparison procedures for an ANOVA.
# glht = general linear hypothesis tests
# install.packages("multcomp")

library(multcomp)
?glht()
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# ALTERNATIVE 2
# The function pairewise.t.test() can be also used to 
# calculate pairwise comparisons between group levels 
# with corrections for multiple testing.
?pairwise.t.test
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# The result is a table of p-values for the pairwise 
# comparisons. 
# Here, the p-values have been adjusted by the 
# Benjamini-Hochberg method.


# TEST VALIDITY
# - homogeneity of variance
# - normality
# Check ANOVA assumptions: test validity?
# The residuals versus fits plot can be used to 
# check the homogeneity of variances.

# 1. Homogeneity of variances
plot(res.aov, 1)

# Points 17, 15, 4 are detected as outliers, which 
# can severely affect normality 
# and homogeneity of variance. It can be useful 
# to remove outliers to meet the test 
# assumptions.

# HINT: It’s also possible to use Bartlett’s test or 
# Levene’s test to check the homogeneity of variances.

library(car)
leveneTest(weight ~ group, data = my_data)

# p-value is not less than the significance level of 0.05. 
# This means that there is no evidence to suggest that 
# the variance across groups is 
# statistically significantly different. 
# Therefore, we can assume the homogeneity of 
# variances in the different treatment groups.


# HINT: what if homogeneity of variance assumption is violated?
# ANOVA test with no assumption of equal variances
oneway.test(weight ~ group, data = my_data)
# Pairwise t-tests with no assumption of equal variances
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

# 2. Normality assumptions
plot(res.aov, 2)
# As all the points fall approximately along this 
# reference line, 
# we can assume normality.

# What test we may use to check normality here?
# Shapiro-Wilk test
# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# ALTERNATIVE 2
# Non-parametric alternative to one-way ANOVA test
# Kruskal-Wallis rank sum test, which can be used 
# when ANNOVA assumptions are not met.

kruskal.test(weight ~ group, data = my_data)

# Post-hoc for Kruskal-Wallis
library(FSA)
dunnTest(weight ~ group, data = my_data,
         method="bh") 

# ----------------------------------------------------------------------------------
# Data transformations and when to use it?
# ----------------------------------------------------------------------------------

# When?
# Violation of assumptions: normality/homogeneity of variance

# Levene test failed?
# Shapiro-Wilk on residuals failed?

# What if transformations do not help?
# Use alternative ( non-parametric ) tests
# Kruskal-Wallis rank sum test

# Couple most popular transformation methods:
# SQUARE ROOT sqrt()
# high values get compressed and low values 
# become more spread out

# LOG log()
# compresses high values more aggressively
# than the square root transformation

# Careful with negatives for sqrt()
# and 0 for log()

# create data frame
df <- data.frame(y1=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8),
                 y2=c(0, 0, 0, 0.48, 0.48, 0.48, 0.48, 0.48, 0.48, 1.2, 1.2, 1.2, 3.21, 3.78, 4.32),
                 x1=c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 7))

# perform Shapiro-Wilk Test on original data
shapiro.test(df$y1)
# after LOG transformation
shapiro.test(log(df$y1))

# perform Shapiro-Wilk Test on original data
shapiro.test(df$y2)
# after SQRT transformation
shapiro.test(sqrt(df$y2))

# Try to avoid data overwrites 
# (might want to create new variable/column)
df$y1_log <- log(df$y1)

# Couple well known examples
# - prices
# - human performance
# - length of chess game
# - Certain physiological measurements (blood pressure)


# ----------------------------------------------------------------------------------
# ANOVA, task 1
# ----------------------------------------------------------------------------------

# Use the Housing Dataset provided in HousingData.csv. 
# Is there a difference  in the housing prices based on 
# the zip code?

setwd('/Users/apple/Desktop/6323_TA/COSC6323_public_files-main/Lesson6_data')
HousingData <- read.csv("HousingData.csv")

# First 3 students with correct solution will 
# get bonus points!

# Post to the text chat: 
# 1) F value 2) P value 
# 3) Df 4) Your conclusion
# Check assumptions. *Maybe transform the data


# ----------------------------------------------------------------------------------
# ANOVA, task 2
# ----------------------------------------------------------------------------------

# Use the Survey Data provided below. 
# Is there a difference in the rating for different weeks?
# What weeks are different? Think of the justification.

rating_scores <- data.frame(
    week = c(rep(1,23),rep(2,14),rep(3,22),rep(4,14),rep(5,13)),
    score = c(c(rep(1,0),rep(2,0),rep(3,0),rep(4,8),rep(5,15)),
             c(rep(1,0),rep(2,0),rep(3,0),rep(4,6),rep(5,8)),
             c(rep(1,1),rep(2,1),rep(3,5),rep(4,9),rep(5,6)),
             c(rep(1,1),rep(2,0),rep(3,0),rep(4,7),rep(5,6)),
             c(rep(1,1),rep(2,0),rep(3,2),rep(4,4),rep(5,6)))
)

# First 3 students with correct solution will 
# get bonus points!

# Post to the text chat: 
# 1) F value 2) P value 
# 3) Df 4) Your conclusion
# Check assumptions.
