# 03/04/2021
# Vitalii Zhukov
# COSC 6323
# Ref.: http://www.sthda.com/english/wiki/one-way-anova-test-in-r

# Plan:
# 0. Project hints, new data available
# 1. Understanding ANOVA
# 2. ANOVA practice task x2
# 3. Homogeneous variances test
# 4. Tukey's procedure
# 5. Exercise review

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
          ylab = "Weight", xlab = "Treatment")

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
res.aov <- aov(weight ~ group, data = my_data)

# Summary of the analysis
summary(res.aov)

# As the p-value is less than the significance level 0.05, we can conclude that 
# there are significant differences between the groups highlighted with “*" in 
# the model summary.
 
# Significant p-value indicates that some of the group means are different, 
# but we don’t know which pairs of groups are different

# As the ANOVA test is significant, we can compute Tukey HSD 
# (Tukey Honest Significant Differences)

TukeyHSD(res.aov)

# Here
# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.

# RESULT
# It can be seen from the output, that only the difference between trt2 and trt1 
# is significant with an adjusted p-value of 0.012.


# ALTERNATIVE 1
# We may also use glht() function from multcomp package to perform multiple comparison 
# procedures for an ANOVA.
# glht = general linear hypothesis tests

library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# ALTERNATIVE 2
# The function pairewise.t.test() can be also used to calculate pairwise 
# comparisons between group levels with corrections for multiple testing.

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# The result is a table of p-values for the pairwise comparisons. 
# Here, the p-values have been adjusted by the Benjamini-Hochberg method.

# TEST VALIDITY
# Check ANOVA assumptions: test validity?
# The residuals versus fits plot can be used to check the homogeneity of variances.

# 1. Homogeneity of variances
plot(res.aov, 1)

# Points 17, 15, 4 are detected as outliers, which can severely affect normality 
# and homogeneity of variance. It can be useful to remove outliers to meet the test 
# assumptions.

# HINT: It’s also possible to use Bartlett’s test or Levene’s test to check 
# the homogeneity of variances.

library(car)
leveneTest(weight ~ group, data = my_data)

# p-value is not less than the significance level of 0.05. 
# This means that there is no evidence to suggest that the variance across groups is 
# statistically significantly different. Therefore, we can assume the homogeneity of 
# variances in the different treatment groups.

# HINT: what if homogeneity of variance assumption is violated?
# ANOVA test with no assumption of equal variances
oneway.test(weight ~ group, data = my_data)
# Pairwise t-tests with no assumption of equal variances
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

# 2. Normality assumptions
plot(res.aov, 2)
# As all the points fall approximately along this reference line, 
# we can assume normality.

# Shapiro-Wilk test
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# ALTERNATIVE 2
# Non-parametric alternative to one-way ANOVA test
# Kruskal-Wallis rank sum test, which can be used when ANNOVA assumptions are not met.

kruskal.test(weight ~ group, data = my_data)


# ----------------------------------------------------------------------------------
# ANOVA, task 1
# ----------------------------------------------------------------------------------

# An experiment to compare the yield of four varieties of rice was conducted. 
# Each of 16 plots on a test farm where soil fertility was fairly homogeneous 
# was treated alike relative to water and fertilizer. Four plots were randomly 
# assigned to the four varieties of rice. The yield in pounds per acre was 
# recorded for each plot. Do the data presented in RiceYield.csv indicate a 
# difference in the mean yield of between the four varieties?
# Visualize the data using boxplot.

setwd('/Users/apple/Desktop/6323_TA/R_scripts/dataset_lesson6')

riceData <- read.csv("RiceYield.csv")
# set factors
varietyFactor <- factor(riceData$variety)
analysis <- aov(riceData$yield ~ varietyFactor)
summary(analysis)

# Since the p-value is less than 0.05, the difference in means is statistically 
# significant.
# Hence there is a difference in the rice yield between the four varieties.

boxplot(riceData$yield~varietyFactor,xlab="Variety",
        ylab="Yield",
        main="Boxplots of yield vs rice variety")
means <- tapply(riceData$yield, varietyFactor, mean)
points( means, pch=8, col="red")

# ----------------------------------------------------------------------------------
# ANOVA, task 2
# ----------------------------------------------------------------------------------

# Use the Housing Dataset provided in HousingData.csv. Is there a difference 
# in the housing prices based on the zip code?

HousingData <- read.csv("HousingData.csv")
# set factors
zipFactor <- factor(HousingData$zip)
analysis <- aov(HousingData$price ~ zipFactor)
summary(analysis)

# Since the p-value is 0.011 < 0.05, the difference in means is statistically 
# significant. Hence there is a difference in the housing prices for different zip code.

# ----------------------------------------------------------------------------------
# Homogeneous variances test (leveneTest)
# ----------------------------------------------------------------------------------

# install.packages("car")    # required for Levene test
library(car)
leveneTest(HousingData$price ~ zipFactor, center=mean)

# Since the p-value < 0.05, we reject H0. Hence, there is significant difference 
# in variance among the different groups.

summary(HousingData$price)

# Levene test for log transformed price
leveneTest(log(HousingData$price) ~ zipFactor, center=mean)

# Since the p-value > 0.05, we fail to reject H0. Hence, there is no difference in 
# variance among the different groups (when we use the log scale).

# Now lets review the ANOVA results for the log-transformed prices:
summary( aov( log(HousingData$price) ~ zipFactor) )

# ----------------------------------------------------------------------------------
# Tukey's procedure
# ----------------------------------------------------------------------------------
a1 <- aov(riceData$yield ~ varietyFactor)
posthoc <- TukeyHSD(x=a1, "varietyFactor", conf.level=0.95)
posthoc

# > Tukey’s procedure declares that μ4 is different only from μ2 and μ3. 
# > We can no longer declare μ4 is different from μ1.
# > Thus in guaranteeing a 0.05 experiment-wise type I error rate, we 
# have lost some power.

# Determine the experiment-wise significance for the housing dataset.
a2 <- aov(log(HousingData$price) ~ zipFactor)
posthoc2 <- TukeyHSD(x=a2, "zipFactor", conf.level=0.95) 
posthoc2

# Tukey’s procedure declares that μ4 is significantly different only from μ3.