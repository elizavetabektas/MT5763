# Section 4.4 

#Task 1
x <- 1:5
# Creates sequence from 1 to 5 
x[3]
# References third element of sequence, i.e. 3
x[c(2, 3, 4)]
# References 2,3,4-th elements of x, i.e. 2 3 4
x[1] <- 17
# Reassigns 1st element to be 17 
x[-3]
# Removes 3rd element i.e. (17 2 4 5)
w <- x[-3]
# Assigns to w the vector x without its 3rd element
y <- c(1, 5, 2, 4, 7)
# creates vector y i.e. 1 5 2 4 7
y[-c(3, 5)]
# vector y without 3rd and 5th elements i.e. 1 5 4
y[c(1, 4, 5)]
# vector y with only 1st, 4th, 5th elements i.e. 1 4 7
i <- 1:3
# creates vector x i.e. 1 2 3
y[i]
# 1 5 2
z <- c(9, 10, 11)
# 9 10 11
y[i] <- z
# reassigns first 3 elements of y to be 9 10 11


# Task 2
order(y)
# puts indices in ascending order
rev(y)
# reverses the order of elements of y
sort(y)
# sorts elements of y in ascending order

# Task 3
y <- 1:10
y^2
# squares every element of y
log(y)
# takes log of every element of y
exp(y)
# takes exponential of every element of y
x <- c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1)
x + y
# adds x y element-wise
x * y
# multiplies x and y element-wise
z <- c(-1, 2.2, 10)
z + y
z * y
# Returns error as the vectors are of different lengths

# Task 4
x <- 1:10
y <- c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1)
x < 4
# Applies <4 operation to each element in x
x[x < 4]
# subset of x where elements are less than 4
y[x < 4]
# subset of y where elements of x are less than 4, works because they are the same length
y > 1 & y <= 4
# elements y where it is greater than 1 and less than or equal to 4
y[y > 1 & y <= 4]
# subset of y where it is greater than 1 and less than or equal to 4
z <- y[y != 3]
# creates vector z that is a subset of elements of y that are not equal to 3


# Task 5
y <- c(5, 4, 3, 2, 10, 5, 4, 3, 2, 1)
factor(y)
factor(y, levels = 1:10)
# Specifies that labels are elements 1 to 10
y <- as.character(c(5, 4, 3, 2, 10, 5, 4, 3, 2, 1))
# creates vector y of characters
factor(y)
# visualises y as a factor, since they are characters they are not in numerical order
factor(y, levels = sort(unique(as.numeric(y))))
# visualises y as a factor, but levels are in order as y is numerical
y <- c("low", "mid", "mid", "high", "low")
factor(y)
# alphabetically shows levels
factor(y, levels = c("low", "mid", "high"))
# we specify order of levels

# Task 6
matrix(1:4, 2, 2)
# creates matrix to fill it by column
matrix(1:4, 2, 2, byrow = TRUE)
# fills the matrix by rows
matrix(0, 3, 4)
# 3x4 matrix of 0's

# Task 7
vals <- c(1, 2, 3, 4, 5, 0.5, 2, 6, 0, 1, 1, 0)
mat <- matrix(vals, 4, 3)
mat[2, 3]
# element in row 2 column 3
mat[1, ]
# first row
mat[, 3]
# third column
mat[-2, ]
# remove second row
mat[c(1, 3), c(2, 3)]
# rows 1 and 3, columns 2 and 3

# Task 8
x1 <- 1:3
x2 <- c(7, 5, 6)
x3 <- c(12, 19, 25)
cbind(x1, x2, x3)
# add the vectors as columns
rbind(x1, x2, x3)
# add the vectors as rows


# Task 9
x <- matrix(1:9, 3, 3)
x
x * 2
# squares every element
x * c(1:3)
# multiplies each row by 1 2 3 in order















