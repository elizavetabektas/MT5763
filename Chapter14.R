# Libraries
library(tidyverse)
library(data.table)
library(microbenchmark)

# Create a large-ish dataset
set.seed(4563)  
someNumbers <- runif(1e6)

# Print object size in mega bytes
print(object.size(someNumbers), 
      standard = "SI", 
      units = "MB")

# Save to a data frame
someNumbers <- matrix(someNumbers, ncol = 100)
someNumbers <- as.data.frame(someNumbers)

# data.table version
IO_data_table <- function(object, path = getwd(), name = "test.csv") {
  # Create IO file path
  filePath <- file.path(path, name)
  
  # Write object to disk
  fwrite(object, filePath, row.names = F)
  
  # Read object back in
  dta <- fread(filePath, header = F)
  
  # Return object
  return(dta)
}


# Readr version
IO_readr <- function(object, path = getwd(), name = "test.csv") {
  # Create IO file path
  filePath <- file.path(path, name)
  
  # Write object to disk
  write_csv(object, filePath)
  
  # Read object back in
  dta <- read_csv(filePath)
  
  # Return object
  return(dta)
}


microbenchmark(
  write_csv(someNumbers, 'someNumbers.csv'), # mean 115.03192 milliseconds
  fwrite(someNumbers, 'someNumbers.csv')) # mean 91.16507 milliseconds

microbenchmark(
  read_csv('someNumbers.csv'), # mean 429.33998 milliseconds
  fread('someNumbers.csv')) # mean 50.14884 milliseconds

someNumbers2 <- runif(1e8)
someNumbers2 <- matrix(someNumbers2, ncol = 10000)
someNumbers2 <- as.data.frame(someNumbers2)

system.time(IO_readr(someNumbers2)) # total 78.347 s
system.time(IO_data_table(someNumbers2)) # this just won't run in a reasonable amount of time, but I'll guess 15 seconds

soCalledHelper <- function(input){
  output <- input
  for(i in 1:500) {
    output <- cbind(output, input)
  }
  return(output)
}

aSimpleFunction <- function() { 
  someData <- read_csv("./test.csv")
  someCalcs <- runif(1e6)%*%runif(1e6)
  charVar <- rep(c("anything", "anythingelse"), 1e3)
  newFile <- soCalledHelper(charVar)  
}

# Start profiler 
aSimpleFunction()
# Stop profiler
