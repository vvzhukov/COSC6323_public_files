library(tidyverse)

# Load data from MASS into a tibble
birthwt <- as_tibble(MASS::birthwt)
?birthwt

# Rename variables
colnames(birthwt) <- c("birthwt.below.2500", 
                       "mother.age",
                       "mother.weight",
                       "race",
                       "mother.smokes",
                       "previous.prem.labor",
                       "hypertension",
                       "uterine.irr",
                       "physician.visits",
                       "birthwt.grams")

# Change factor level names
birthwt <- birthwt %>%
    mutate(race = recode_factor(race, `1` = "white", `2` = "black", `3` = "other")) %>%
    mutate_at(c("mother.smokes", "hypertension", "uterine.irr", "birthwt.below.2500"),
              ~ recode_factor(.x, `0` = "no", `1` = "yes"))

# Create boxplot showing how birthwt.grams varies between
# history of hypertension
qplot(x = hypertension, y = birthwt.grams,
      geom = "boxplot", data = birthwt,
      xlab = paste("Mother hypertension, n = ", nrow(birthwt)), 
      ylab = "Birthweight (grams)",
      fill = I("lightblue"))

# Summary data
# Notice the consistent use of round() to ensure that our summaries 
# do not have too many decimal values

birthwt %>%
    group_by(hypertension) %>%
    summarize(num.obs = n(),
              mean.birthwt = round(mean(birthwt.grams), 0),
              sd.birthwt = round(sd(birthwt.grams), 0),
              se.birthwt = round(sd(birthwt.grams) / sqrt(num.obs), 0))

# 

birthwt.t.test <- t.test(birthwt.grams ~ hypertension, data = birthwt)
birthwt.t.test

birthwt.t.test$p.value   # p-value
# A large p-value (> 0.05) indicates weak evidence against the null hypothesis, so we fail to reject it


