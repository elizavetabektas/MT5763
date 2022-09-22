set.seed(3245)
# create data
x <- runif(1000)
y <- 20 + 2*x + rnorm(1000, 0, 1)

regData <- data.frame(x, y)

plot(x, y, pch = 20, col = 'purple', cex = 3)



badBootBrother2 <- function(inputData, nBoot){

  X <- cbind(1, scale(inputData$x, scale = F))
  Y <- scale(inputData$y, scale = F)
  scaleData <- as.matrix(cbind(X, Y))
  
  bootResults <- array(dim=c(nBoot, 2))
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- scaleData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    Xmat <- bootData[,1:2]
    Ymat <- bootData[,3]
    
    # fit the model under this alternative reality
    # Changed the lm part to matrix form
    
    beta <- solve(t(Xmat)%*%Xmat)%*%t(Xmat)%*%Ymat
    
    bootResults[i,] <- beta
    
  } # end of i loop
  
  bootResults
  
}

