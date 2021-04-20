# 02/04/2021
# Vitalii Zhukov
# COSC 6323

# Tests for Association in R; chisq.test()

# test(x, y = NULL) – A basic chi-squared test is carried out on a matrix or data frame. 
# If it provides x as a vector, a second vector can be supplied. If x is a single vector 
# and y is not given, a goodness of fit test is carried out.

# correct = TRUE – It applies Yates’ correction if the data forms a 2 n 2 contingency table.

# p = – It is a vector of probabilities for use with a goodness of fit test. If p is not given, 
# the goodness of fit tests that the probabilities are all equal.

# p = FALSE – If TRUE, p is re-scaled to sum to 1. For use with the goodness of fit tests.

# p.value = FALSE – If set to TRUE, a Monte Carlo simulation calculates p-values.

# B = 2000 – The number of replicates to use in the Monte Carlo simulation.

# EXAMPLE
library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency table 

# problem
# Test the hypothesis whether the students smoking habit is independent of their exercise 
# level at .05 significance level.

# solution
chisq.test(tbl)

# answer
# As the p-value 0.4828 is greater than the .05 significance level, we do not reject 
# the null hypothesis that the smoking habit is independent of the exercise level of the students.

# enhanced solution
# The warning message found in the solution above is due to the small cell values in the contingency 
# table. To avoid such warning, we combine the second and third columns of tbl, and save it in 
# a new table named ctbl. Then we apply the chisq.test function against ctbl instead

ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 
ctbl 

chisq.test(ctbl) 

