# 01/22/2021
# Vitalii Zhukov
# COSC 6323

# ---- Operators

# Arithmetic Operators
# +, -, *, /, %%, %/%, ^
# %%   Give the remainder of the first vector with the second
# %/%  The result of division of first vector with second (quotient)

?'+'
# Examples:
v1 <- c(1,2,3)
v2 <- c(4,5,6)

v3 <- v1 + v2

v3 <- v3 - v2

v3 <- v1 * v2

v3 <- v1 ^ v2

# Relational Operators
# >, <, ==, ,<=, >=, !=

?'<'
# Examples:
v1 > v2

v3 == c(1, 32, 729)

# Logical Operators 
# &, |, ! (AND, OR, NOT)
# &&, || uses only the first element

?'&'
# Examples

c(TRUE, FALSE, TRUE) & c(TRUE, TRUE, FALSE)
c(TRUE, FALSE, TRUE) && c(TRUE, TRUE, FALSE)

c(FALSE, FALSE, FALSE) | c(FALSE, TRUE, TRUE)
c(FALSE) || c(FALSE, TRUE, TRUE)

c(FALSE) & c(FALSE, TRUE, TRUE)
c(FALSE) && c(FALSE, TRUE, TRUE)

# Assignment Operators
# Already discussed in Lesson1_variables.R
a <- c(1)
a <- 1
a = 1
1 -> a
a <<- c(1)
1 ->> a

# Other Operators
# 
?':'
?'%in%'
?'%*%'

# Examples
v <- 1:10
v

2 %in% v
32 %in% v

M <- matrix( c(1:6), nrow = 2, ncol = 3, byrow = TRUE)
M %*% t(M)
