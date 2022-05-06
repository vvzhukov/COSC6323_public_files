# Created 04/30/2021
# Updated 04/29/2022
# Vitalii Zhukov
# COSC 6323
# Ref.: 

# http://www.sthda.com/english/wiki/comparing-means-in-r
# http://www.sthda.com/english/wiki/one-sample-wilcoxon-signed-rank-test-in-r
# http://www.r-tutor.com/elementary-statistics/non-parametric-methods
# http://personal.psu.edu/drh20/astrostatistics/KavalurIndia2007/np.html
# https://www.r-bloggers.com/2012/02/beware-the-friedman-test/
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/friedman.test
# http://www.sthda.com/english/articles/38-regression-model-validation/156-bootstrap-resampling-essentials-in-r/
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know



# PLAN
# I. One-sample Wilcoxon signed rank test  
#       (One-sample nonparametric Test)
# II. Mann-Whitney-Wilcoxon Test 
#       (Unpaired Two-Samples Wilcoxon Test)
# III. Paired Samples Wilcoxon Test 
#       (non-parametric)
# IV. Kruskal-Wallis Test 
#       (one-way ANOVA)
# V. Friedman test
# VI. Bootstrapping
# VII. Word frequencies / clouds



# A statistical method is called non-parametric if it 
# makes no assumption on the population distribution 
# or sample size.

# I. Wilcoxon signed rank test (One-sample nonparametric Test)
# The one-sample Wilcoxon signed rank test is a 
# non-parametric alternative to one-sample t-test 
# when the data cannot be assumed to be normally distributed.

# IMPORTANT assumption:
# - the data should be distributed symmetrically (median) 

#if(!require(devtools)) install.packages("devtools")

# HOW-TO
# wilcox.test(x, mu = 0, alternative = "two.sided")
# x: a numeric vector containing your data values
# mu: the theoretical mean/median value (default - 0). 
# alternative: the alternative hypothesis. 
# Allowed value is one of “two.sided” (default), “greater” or “less”.

# Here, we will use an example data set containing 
# the weight of 10 mice.
set.seed(1234)
my_data <- data.frame(
    name = paste0(rep("M_", 10), 1:10),
    weight = round(rnorm(10, 20, 2), 1)
)

head(my_data, 10)

summary(my_data$weight)

library(ggpubr)
ggboxplot(my_data$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

# We want to know, if the average weight of the 
# mice differs from 25g (two-tailed test)?

# One-sample wilcoxon test
res <- wilcox.test(my_data$weight, mu = 19)
# Printing the results
res 

# print only the p-value
res$p.value

# reject the null hypothesis and conclude that the 
# average weight of the mice is significantly 
# different from 25g with a p-value = 0.005793

# test whether the median weight of mice is 
# less than 25g (one-tailed test)
wilcox.test(my_data$weight, mu = 25,
            alternative = "less")

# test whether the median weight of mice is 
# greater than 25g (one-tailed test)
wilcox.test(my_data$weight, mu = 25,
            alternative = "greater")

# Exercise #1
# Given 15 measures of the detail length, identify if 
# it is smaller than 8 inches
details <- c(8.2, 7.5, 8.1, 7.7, 8.2,
             7.7, 8.4, 8.5, 7.0, 8.3,
             8.6, 8.1, 8.0, 7.8, 7.9)
# (1) what are main assumptions?
# (2) which test we are using and why? 
# Answer should include: p-value, conclusion and 
# short answers to the questions (1) and (2) 

wilcox.test(details, mu = 8,
            alternative = "less")


# II. Mann-Whitney-Wilcoxon Test 
# (unpaired Two-Samples Wilcoxon Test)
# The unpaired two-samples Wilcoxon test 
# (also known as Wilcoxon rank sum test or Mann-Whitney test) 
# is a non-parametric alternative to the 
# unpaired two-samples t-test

# Here, we’ll use an example data set, which contains 
# the weight of 18 individuals (9 women and 9 men):

# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 
                  64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 
                73.3, 67.3, 61.3, 62.4) 

# Create a data frame
my_data <- data.frame( 
    group = rep(c("Woman", "Man"), each = 9),
    weight = c(women_weight,  men_weight)
)

print(my_data)

library(dplyr)
group_by(my_data, group) %>%
    summarise(
        count = n(),
        median = median(weight, na.rm = TRUE),
        IQR = IQR(weight, na.rm = TRUE)
    )

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

res <- wilcox.test(women_weight, men_weight)
res

res$p.value
# We can conclude that men’s median weight is 
# significantly different from women’s median 
# weight with a p-value = 0.02712.

# test whether the median men’s weight is less 
# than the median women’s weight
wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")
# test whether the median men’s weight is greater 
# than the median women’s weight
wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "greater")


# Exercise #2
# Given 15 measures of 2 details sets length, identify if 2nd
# detail set is larger than the first.
detail1 <- c(8.2, 7.5, 8.1, 7.7, 8.2,
             7.7, 8.4, 8.5, 7.0, 8.3,
             8.6, 8.1, 8.0, 7.8, 7.9)

detail2 <- c(8.3, 7.2, 8.2, 7.9, 8.6,
             7.4, 8.6, 7.5, 7.5, 8.4,
             8.8, 8.3, 8.0, 7.9, 8.1)

# Answer should include: stat, p-value, conclusion
wilcox.test(detail1, detail2,
            exact = FALSE, alternative = "greater")



# III. Paired Samples Wilcoxon Test (non-parametric)
# The paired samples Wilcoxon test 
# (also known as Wilcoxon signed-rank test) 
# is a non-parametric alternative to paired t-test used 
# to compare paired data.
# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 
           196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 
          427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
    group = rep(c("before", "after"), each = 10),
    weight = c(before,  after)
)

library("dplyr")
group_by(my_data, group) %>%
    summarise(
        count = n(),
        median = median(weight, na.rm = TRUE),
        IQR = IQR(weight, na.rm = TRUE)
    )

# Plot weight by group and color by group
library(ggpubr)
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")

# install.packages("PairedData")
# Subset weight data before treatment
before <- subset(my_data,  group == "before", weight,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(my_data,  group == "after", weight,
                drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

# paired-sample Wilcoxon test
# method 1, data in two separate vectors:
wilcox.test(before, after, paired = TRUE)

# method 2, data in a data frame:
res <- wilcox.test(weight ~ group, data = my_data, paired = TRUE)

res$p.value
# Median weight of the mice before treatment 
# is significantly different from the median 
# weight after treatment with a p-value = 0.001953


# IV. Kruskal-Wallis Test (one-way ANOVA)
# Extends the two-samples Wilcoxon test in the 
# situation where there are more than two groups. 

my_data <- PlantGrowth
head(my_data)
summary(PlantGrowth)

levels(my_data$group)

# Reorder levels if you need to do so:
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

# Stat by group
library(dplyr)
group_by(my_data, group) %>%
    summarise(
        count = n(),
        mean = mean(weight, na.rm = TRUE),
        sd = sd(weight, na.rm = TRUE),
        median = median(weight, na.rm = TRUE),
        IQR = IQR(weight, na.rm = TRUE)
    )

library("ggpubr")
# BOXPLOT. Plot weight by group and color by group
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", 
          palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# MEAN PLOTS. Plot weight by group
# Add error bars: mean_se
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

kruskal.test(weight ~ group, data = my_data)
# As the p-value is less than the significance 
# level 0.05, we can conclude that there are 
# significant differences between the treatment groups.

# Which pairs are different??
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH", exact=FALSE)
# The pairwise comparison shows that, only trt1 and trt2 
# are significantly different (p < 0.05).


# V. Friedman test
# Similar to the parametric repeated measures ANOVA, 
# it is used to detect differences in treatments across 
# multiple test attempts

# Example 1
## Hollander & Wolfe (1973), p. 140ff.
## Comparison of three methods ("round out", "narrow angle", and
##  "wide angle") for rounding first base.  For each of 18 players
##  and the three method, the average time of two runs from a point on
##  the first base line 35ft from home plate to a point 15ft short of
##  second base is recorded.

RoundingTimes <-
    matrix(c(5.40, 5.50, 5.55,
             5.85, 5.70, 5.75,
             5.20, 5.60, 5.50,
             5.55, 5.50, 5.40,
             5.90, 5.85, 5.70,
             5.45, 5.55, 5.60,
             5.40, 5.40, 5.35,
             5.45, 5.50, 5.35,
             5.25, 5.15, 5.00,
             5.85, 5.80, 5.70,
             5.25, 5.20, 5.10,
             5.65, 5.55, 5.45,
             5.60, 5.35, 5.45,
             5.05, 5.00, 4.95,
             5.50, 5.50, 5.40,
             5.45, 5.55, 5.50,
             5.55, 5.55, 5.35,
             5.45, 5.50, 5.55,
             5.50, 5.45, 5.25,
             5.65, 5.60, 5.40,
             5.70, 5.65, 5.55,
             6.30, 6.30, 6.25),
           nrow = 22,
           byrow = TRUE,
           dimnames = list(1 : 22,
                           c("Round Out", "Narrow Angle", "Wide Angle")))

friedman.test(RoundingTimes)
## => strong evidence against the null that the 
## methods are equivalent with respect to speed

# Example 2
# The Number of Breaks in Yarn during Weaving
?warpbreaks
wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)
wb
friedman.test(wb$x, wb$w, wb$t)
friedman.test(x ~ w | t, data = wb)


# VI. Bootstrapping
# bootstrap re sampling method can be used to measure 
# the accuracy of a predictive model. Additionally, 
# it can be used to measure the uncertainty associated 
# with any statistical estimator.

# Following example uses a bootstrap with 100 re samples 
# to test a linear regression model:
library(tidyverse)
library(caret)

# Load the data
?swiss
data("swiss")
# Inspect the data
sample_n(swiss, 3)

# Define training control
?caret::train
train.control <- trainControl(method = "boot", number = 100)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

# Creating a function that returns the regression model 
# coefficients:

model_coef <- function(data, index){
    coef(lm(Fertility ~., data = data, subset = index))
}
model_coef(swiss, 1:47)

# Compute the standard errors of 500 bootstrap 
# estimates for the coefficients
library(boot)
?boot
# Generate R bootstrap replicates of a statistic 
# applied to data.
boot(swiss, model_coef, 500)

# Here:
# 'original' column corresponds to the regression coefficients
# 'std.error' associated standard errors

# For example 95% CI (b =- 2*SE(b)):
# SE for Agriculture (t2*) is 0.06
# then the lower limit:
-0.172 - (2*0.06)
# and upper:
-0.172 + (2*0.06)

# So there is 95% chance that interval [-0.292, -0.052] 
# will contain the true value of the coefficient

summary(lm(Fertility ~., data = swiss))$coef
# LM gives you different results as it relies on some 
# assumptions regarding the data

# bootstrap approach does not rely on any of these 
# assumptions made by the linear model, and so it 
# is likely giving a more accurate estimate of the 
# coefficients standard errors than is the summary() function.

#  Spearman 
data(mtcars)
?mtcars
?cor.test
cor.test(mtcars$mpg, mtcars$hp, method="spearman")


# VII. Word frequencies / clouds
# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
?Corpus
docs <- Corpus(VectorSource(text))
inspect(docs)

# Transform text
toSpace <- content_transformer(function (x , pattern ) 
    gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("get", "can", 
                                    "assignments",
                                    "going",
                                    "though",
                                    "got",
                                    "will",
                                    "might")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

# Build a text document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)
# Which words are associated with “freedom” in I 
# have a dream speech
findAssocs(dtm, terms = "freedom", corlimit = 0.3)

# Plot 10 most frequent words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
