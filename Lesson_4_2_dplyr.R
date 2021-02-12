# 02/11/2021
# Vitalii Zhukov
# COSC 6323

# DPLYR (https://dplyr.tidyverse.org), part of tidyverse
# mutate, select, filter, summarise, arrange
# all combined by group_by

# CHEAT SHEET: https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

library(dplyr)

# simple case of filtering
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