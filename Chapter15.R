library(parallel)
detectCores()
# 4

## Copy of code in notes

# Define a function that we will call several times
fit_model <- function(t=NULL) {
  x <- rnorm(100)
  df <- data.frame(x = x,
                   y = 2*x + rnorm(100))
  return(lm(y ~ x, data = df))
}

# lapply version
system.time(out <- lapply(1:1e4, fit_model))


# mclapply version - non-windows only 
system.time(out <- mclapply(1:1e4, fit_model, 
                            mc.cores = 4))


cl <- makeCluster(spec = 4, type = "PSOCK",
                  setup_strategy = "sequential")

# Execute expression in each process
clusterEvalQ(cl = cl, expr = {
  library(MASS)
  sqrt(9)
})

# Define some data in the master process
set.seed(101) # for reproducibility
someData <- rnorm(100) 
somePower <- 2

# Export someData / somePower to each subprocess
clusterExport(cl = cl, 
              varlist = c("someData", "somePower"))

# Use exported objects within each subprocess
sumData <- clusterEvalQ(cl = cl, expr = {
  sum(someData^somePower)
})
sumData

# Use the same example as mclappy
system.time(out <- parLapply(cl = cl, X = 1:1e4,
                             fun = fit_model))

stopCluster(cl)


# Function to be evaluated
my_func <- function(x, corr) {
  # corr = correlation between x1 and x2
  # Generate correlated covariates
  # Requires the MASS package (mvrnorm)
  covariates <- mvrnorm(n = 100, mu = c(0, 0), 
                        Sigma = matrix(c(1, corr, corr, 1),
                                       ncol = 2,
                                       nrow = 2))
  x1 <- covariates[ ,1]
  x2 <- covariates[, 2]
  
  # Generate some outcome
  y <- 10 + 2*x1^myPower - 4*x2^myPower + rnorm(100)
  
  # Store in data frame
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  
  # Fit model
  lm(y ~ ., data = df)
}

# 1. Create / register a cluster of n workers.
cl <- makeCluster(spec = 4, type = "PSOCK",
                  setup_strategy = "sequential")

# 2. Execute any pre-processing code needed by each worker.
myPower <- 2
clusterExport(cl = cl, varlist = "myPower")
clusterEvalQ(cl = cl, expr = {library(MASS)})

# 3. Use the appropriate `par*apply` function.
out <- parLapply(cl = cl, X = 1:4,
                 fun = my_func, corr = 0.8)

# 4. Close / stop the cluster.
stopCluster(cl)





