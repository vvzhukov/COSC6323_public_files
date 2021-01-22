# 01/22/2021
# Vitalii Zhukov
# COSC 6323

## STRING
# String manipulation
# Concat strings:
?paste
paste("Hey", "there!", sep=" ")


# Format output
?format
result <- format(12.12123123123, digits = 5)
result

format("Hello Center", width = 20, justify = "r")

?nchar
nchar("How many characters here?")

?toupper
toupper("you will see upper case here")
tolower("AND LOWER CASE HERE")

?substring
substring("Elephant", 3,5)

## VECTOR
# Vectors sequencing
1:10 

1.5:10.5

1.5:10.3 # Last element out of bound

?seq
seq(3,6, by=0.3)

# What if we try to combine different data types in one vector?
s <- c('black','red',0,TRUE)
s
class(s)

# Accessing vector elements
myVector <- c("First", "Second", "Third", "Fourth")

# 1. by position
myVector[1]
myVector[c(1,2)]

# 2. by logical indexing
myVector[c(TRUE, FALSE, FALSE, TRUE)]

# 3. by negative indexing
myVector[c(-1,-3)]
myVector[c(-3,-1)] # in both cases it preserves the order

# What is element recycling?

v1 <- c(1,2,3,4,5,6)
v2 <- c(1,2)
# V2 becomes c(1,2,1,2,1,2)

v1+v2
v1-v2


# Sorting
?sort
sort(c(2,4,6,1,3), decreasing = TRUE)

## DATA FRAMES
# The column names should be non-empty
# The row names should be unique
# The data stored in a data frame can be of numeric, factor or character type
# Each column should contain same number of data items


BMI <- 	data.frame(
    gender = c("Male", "Male","Female"), 
    height = c(152, 171.5, 165), 
    weight = c(81,93, 78),
    Age = c(42,NA,26)
)

# Get the structure of the BMI
str(BMI)

# Get summary
summary(BMI)

# Extract data
BMI$Age

# Create a new data frame
data.frame(BMI$Age, BMI$height)

# Extract first two rows and all columns
BMI[c(1,2),]

# Extract first two columns and all rows
BMI[,c(1,2)]

# Expand data frame
# add column
BMI
BMI$id <- c(21,22,23)
BMI

# add row


#combine two data frames
?rbind
DF1 <- data.frame(
    first = c(1,2,3),
    second = c(4,5,6)
)

DF2 <- data.frame(
    first = c(7,8,9),
    second = c(10,11,12) 
)

DF3 <- rbind(DF1, DF2)
DF3

