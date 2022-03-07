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

setwd('/Users/apple/Desktop/6323_TA/Git_code/Lesson6_data')

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
# significant. Hence there is a difference in the housing prices for different 
# zip code.
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

# Since the p-value > 0.05, we fail to reject H0. Hence, there is no difference 
# in variance among the different groups (when we use the log scale).

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