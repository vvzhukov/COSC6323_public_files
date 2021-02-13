# 02/11/2021
# Vitalii Zhukov
# COSC 6323

# Plan:
# 1. HYPOTHESIS TEST ON P
# 2. HYPOTHESIS TEST ON SIGMA^2
# 3. DPLYR data manipulation
# 4. KS fitting
# 5. Gephi overview
# 6. HW discussion?

# HYPOTHESIS TEST ON P

# Winter has come to Westeros. Winterfell needs new warriors.
# One of the Ravens told the King that newly born babies are more 
# likely to become warriors than any other profession so he doesn't 
# need to change his offensive strategy.
# However King is wise and he assigned Hand of the King to review the 
# problem from the statistical point of view.

# A random sample found 13,173 warriors (soldiers) were born among 25,468 
# newborn children. The sample proportion of soldiers 
# was 0.5172. Is this sample evidence that the birth of 
# soldier is more common than the birth of non-soldier in the 
# entire population? What should Lord hand include in the report for the King?

# The hypotheses are
# H0 :p≤0.5
# H1 :p>0.5
# Note that this is a one-tailed test.

# The test statistic is
p_hat = 13173/25468
p0 = 0.5
n = 25468
z = (p_hat - p0)/sqrt(p0*(1-p0)/n)

# The p value is
pnorm(z, lower.tail = FALSE)

# Since the p value is less than 0.05, we can reject H0.

# We would say that there is sufficient evidence to 
# conclude soldiers are more common than other professions in the 
# entire population. Raven was right!

# Alternative solution:
prop.test(13173, 25468, p=0.5, alt="greater",
            correct=FALSE)

# HYPOTHESIS TEST ON SIGMA^2


# We have a pump at the gas station which seems to 
# fill not consistent amount of fuel.
# To monitor the accuracy of the pump, a sample of 10 
# canisters was taken. Each canister volume is 14 gallons.

# Technical requirement is not only that the pump will 
# fill canisters with 14 gallons, but also maintain 
# a consistent amount of fuel being put into the canister.

# The variance should be 0.01 and the measured variance
# is SS = 0.3211

# We want to test the hypotheses
# H0: sigma^2 = 0.01 vs sigma^2 > 0.01

# The rejection is based on the statistic
# X^2 = SS / 0.01

# X^2 = 32.11
# Now we compare it with a Chi-Squared distribution with 
# 9 degrees of freedom:

# Critical region value is:
qchisq(0.95,9)


# Since the computed statistic value 32.11 is greater 
# than the critical value 16.91, the null hypothesis 
# is rejected.

# The pump should be replaced.