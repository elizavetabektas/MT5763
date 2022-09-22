set.seed(3245)
# create data
x <- runif(1000)
y <- 20 + 2*x + rnorm(1000, 0, 1)

regData <- data.frame(x, y)

plot(x, y, pch = 20, col = 'purple', cex = 3)




badBootBrother <- function(inputData, nBoot){
  
  for(i in 1:nBoot){
    
    # resample our data with replacement
    bootData <- inputData[sample(1:nrow(inputData), nrow(inputData), replace = T),]
    
    # fit the model under this alternative reality
    bootLM <- lm(y ~ x, data = bootData)
    
    # store the coefs
    if(i == 1){
      
      bootResults <- matrix(coef(bootLM), ncol = 2)
      plot(inputData$x, inputData$y)
      abline(coef(bootLM), col = 'grey')
      
    } else {
      
      bootResults<- rbind(bootResults, matrix(coef(bootLM), ncol = 2))
      
      abline(coef(bootLM), col = 'grey')
      
    }
    
    print(i)
    
  } # end of i loop
  
  bootResults
  
}
