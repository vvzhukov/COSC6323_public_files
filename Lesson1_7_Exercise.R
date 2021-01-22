# 01/22/2021
# Vitalii Zhukov
# COSC 6323

# First sample task is not mandatory
# Please feel free to ask your questions

# 1. Upload the mtcars dataset
data(mtcars)

# 2. Filter all 8 cylinders cars
subset(mtcars, cyl == 8)

# 3. Save as a CSV file 8cylCars.csv
write.csv(subset(mtcars, cyl == 8),"8cylCars.csv")

# 4. Filter all cars with horse powers >150 save result as a dataframe myMtcars
myMtcars <- subset(mtcars, hp > 150)

# 5. Based on the myMtcars data plot a boxplot, showing the Number of gears vs horse powers
boxplot(hp ~ gear, data = myMtcars, xlab = "Number of gears",
        ylab = "HP")