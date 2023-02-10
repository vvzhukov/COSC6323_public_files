# Created: 02/11/2021
# Updated: 02/10/2023
# Vitalii Zhukov
# COSC 6323

# DPLYR (https://dplyr.tidyverse.org), part of tidyverse
# mutate, select, filter, summarise, arrange
# all combined by group_by
# CHEAT SHEET: https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

# Plan
# 1. DPLYR data manipulation (if we still have time)
# 2. KS fitting (if we have time)

library(dplyr)

# simple case of filtering
str(starwars)

starwars %>% 
    filter(species == "Droid")

# selecting multiple rows by condition
starwars %>% 
    select(name, ends_with("color"))

# dynamic variables 
starwars %>% 
    mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
    select(name:mass, bmi)

# arranging
starwars %>% 
    arrange(desc(mass))

# complex syntax
starwars %>%
    group_by(species) %>%
    summarise(
        n = n(),
        mass = mean(mass, na.rm = TRUE)
    ) %>%
    filter(
        n > 1,
        mass > 50
    )


# KS fitting
x <- rnorm(50)
y <- runif(30)

?ks.test()

# Do x and y come from the same distribution?
ks.test(x, y)

# If y is numeric, a two-sample test of the null hypothesis that x and y 
# were drawn from the same continuous distribution is performed.

# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact

