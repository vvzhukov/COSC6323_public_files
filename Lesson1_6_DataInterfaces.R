# 01/22/2021
# Vitalii Zhukov
# COSC 6323

# File system
getwd()
setwd('/Users/apple/Desktop/6323_TA/COSC6323_public_files-main/')

# CSV
data <- read.csv("sampleCsv.csv")
class(data)

# filter all IT guys and save data to the file
dataIT <- subset(data, dept=="IT")
write.csv(dataIT,"output.csv")

# In the same manner R works with:

# EXCEL
install.packages("xlsx")
library("xlsx")
?'read.xlsx'

# XML
install.packages("XML")
library("XML")
??'xmlParse'

# And much more... JSON, web data, Databases....

# Built-in datasets
data(mtcars)
?mtcars

str(mtcars)
colnames(mtcars)

# Couple words about plotting

# Pie chart
pie(mtcars$mpg[1:5], rownames(mtcars)[1:5])

# Bar chart
barplot(mtcars$mpg[1:5], ylim = c(0,26))
?barplot

# Boxplot
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

# Histogram
hist(mtcars$mpg[1:5],xlab = "MPG",col = "yellow",border = "blue")

# Line graphs
plot(mtcars$mpg[1:5])
plot(mtcars$mpg[1:5],type = "o")

# Scatterplots
input <- mtcars[,c('wt','mpg')]

plot(x = input$wt,y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),		 
     main = "Weight vs Milage"
)


