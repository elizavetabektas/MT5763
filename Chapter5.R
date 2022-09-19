

# 5.1.1 For loops

# Task 1
for(i in 1:10) {
  ## generate 10 random numbers
  ## uniformly between 0 and 1
  x <- runif(10)
  
  ## calculate the mean
  x <- mean(x)
  
  ## print the mean to the screen
  print(x)
}

for (i in 1:10) print(mean(runif(10)))

# Task 2
transpose <- function(matrix){
  Tmatrix <- matrix(0, nrow(matrix), ncol(matrix))
  for (i in 1:ncol(x)){
    for (j in 1:nrow(x)){
      Tmatrix[i,j] <- matrix[i,j]
    }
  }
  return(Tmatrix)
}

x <- matrix(1:9, 3, 3)
transpose(x)

# 5.1.2 While loops

# Task 1
# Write a while loop that generates a uniform random number between 0 and 1 (use the runif() function), until a number of > 0.9 is obtained.
i <- 0
while (i <= 0.9){
  i <- runif(1)
  print(i)
}

# 5.2 Conditional statements

# Task 1
# Generate two uniform random numbers between 0 and 1, called x and y.
# Write a nested if/else statement that prints out whether x is > 0.5,
# and then whether y is > 0.5.
x <- runif(1)
y <- runif(1)
if (x>0.5){
  print(x)
  if (y>0.5){
    print(y)
  }
}

# 5.3 User defined functions 

# Task 1
paste("Here is a number:", 1:10)
# Prints for every number in sequence
paste(1:10, collapse = " + ")
# Prints sequence together with separator +
paste("Here are some numbers:", paste(1:10, collapse = ", "))
# Prints one statement, collapsing sequence of 1 to 10
x <- 1:10; paste("The sum of the first", length(x), "numbers is given by", paste(1:10, collapse = " + "), "=", sum(x))

# Task 2
whoknows <- function(x) {
    for(i in 2:x) {
        if(x %% i == 0){ 
            return(i) 
        }
    }
}
# prints first number that divides integer x



# Task 2
## function to check whether x is a prime number
checkprime <- function(x) {
    ## check `x` variable is present
    stopifnot(!missing(x))
    ## check `x` is numeric
    stopifnot(is.numeric(x))
    ## check `x` is of length one
    stopifnot(length(x) == 1)
    ## check `x` is a whole number
    stopifnot(x %% 1 == 0)
    ## check `x` is larger than 1
    stopifnot(x >= 2)
    ## check whether x can be divided by something
    ## other than itself to leave a whole number
    for(i in 2:x) {
        if(x %% i == 0 & i != x) {
            return(paste(x, "is not a prime number")) 
        }
    }
    return(paste(x, "is a prime number"))
}

checkprime(3891)
checkprime(7103)
checkprime(7919)
checkprime(10535)


# Task 3
x <- matrix(1:12, 3, 4)
# Use apply() to return the range of each column of the matrix x defined above.
# Return as a 2  ×  4 matrix.
ranges <- t(apply(x, 2, range))


# 5.4.2 lapply() and sapply()
x <- list(matrix(1:4, 2, 2), matrix(1:9, 3, 3))
# lapply() returns list
# sappply() returns simpler object if possible

# 5.4.3 tapply()
# Task
summary(PlantGrowth)
print('means by weight')
tapply(PlantGrowth$weight, PlantGrowth$group, mean)
print('standard deviations by weight')
tapply(PlantGrowth$weight, PlantGrowth$group, sd)



