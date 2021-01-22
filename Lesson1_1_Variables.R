# 01/22/2021
# Vitalii Zhukov
# COSC 6323

# R - statistical analysis, graphics representation and reporting

# ---- HELP

help("print")
?print
?help # help for the help :)

# ---- VARIABLES
# Name may include: letters, numbers and the dot or underline characters
# Should start from letter or dot (not followed by number)

# Types of assignment
?"<-"
# '<-', '->', "=", '<<-', '->>'

my_String. <- "Hello, World!"
"Hello, World!" -> my_String.2
my_String.3 = "Hello, World!"

# HINT: 
# '<-', '->' and '=' into current environment
# '<<-', '->>'
# Example:

median(x = 1:10)
?median
x

# but this will work:
median(x <- 1:10)
x    

my_String.
print(my_String.)
class(my_String.)

# In R, a variable itself is not declared of any data type, rather it gets the data type of the 
# R - object assigned to it. So R is called a dynamically typed language, which means that we can 
# change a variable’s data type of the same variable again and again when using it in a program

x.1 <- "Healthy"
x.2 <- 36.6
x.3 <- 36L
.secret <- "Celsius"
cat("Here are my variables classes: ", class(x.1), ", ",class(x.2), ", ",class(x.3))

# How to check the env for existing variables?
?ls()
ls()

# We may apply filter:
ls(pattern = "x.")

# And even take a look at the hidden variables (those which start with '.')
print(ls(all.name = TRUE))

# Deleting variables:
?rm
rm(x.1)
x.1

# Deleting all variables
rm(list = ls())

