# 01/22/2021
# Vitalii Zhukov
# COSC 6323

# ---- DATA TYPES

# Most frequently used data types:
# Vectors, Lists, Matrices, Arrays, Factors, Data Frames

# 1. Vectors
# Simplest atomic objects! Others are built on top of vectors.

# Logical
v <- TRUE
print(class(v))
v

# Numeric
v <- 23.5
print(class(v))
v

# Integer
v <- 2L
print(class(v))
v

# Complex
v <- 2+5i
print(class(v))
v

# Character
v <- "Char"
print(class(v))
v

# Raw
v <- charToRaw("Char")
print(class(v))
v

# COMBINE function c()
?c()

# Create a new vector:
v_color_scheme <- c('r','g','b')
v_color_scheme
class(v_color_scheme)

# Get the class of the vector.
print(class(v_color_scheme))

# 2. Lists
# Many objects of different type

# Create a list
?list
my_list <- list(c(1,2,3),21.3,"RGB")

# Print the list
print(my_list)
class(my_list)

# 3. Matrices
# Two-dimensional rectangular data set
?matrix
M <- matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)

N <- matrix( c('a','a','b','c','b','a'), nrow = 3, ncol = 2, byrow = TRUE)
print(N)

# 4. Arrays
# Any-dimensional data set
?array
a <- array(c('r','g', 'b'),dim = c(3,3,3))
print(a)

# 5. Factors
?factor

# Create a vector
flag_colors <- c(rep('White',13),'Old Glory Red','Old Glory Blue')
flag_colors

# Create a factor object
factor_flag <- factor(flag_colors)

# Print the factor.
print(factor_flag)
print(nlevels(factor_flag))

# 6. Data Frames
?data.frame
# Here comes the King

# Create the data frame
BMI <- 	data.frame(
    gender = c("Male", "Male","Female"), 
    height = c(152, 171.5, 165), 
    weight = c(81,93, 78),
    Age = c(42,38,26)
)
print(BMI)

