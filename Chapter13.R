# 13 Computer intensive Inference

# 13.1 Monte Carlo Simulation

# Task 1

#loading pacakges
library(tidyverse)

# Specify number of throws and radius
num_throws <- seq(from=50, to=5000, by=10)
r <- 0.5

# Loop across different number of throws
set.seed(6017) # for reproducibility
pi_hat_vec=c() # to store estimates
for (num_throw in num_throws)
{
  # Simulate throws
  x <- runif(num_throw, min = 0, max = 1)
  y <- runif(num_throw, min = 0, max = 1)
  
  # Estimate pi
  pi_hat <- 4*(sum((x-0.5)^2 + (y-0.5)^2 < r^2) / num_throw)
  
  # Add estimate to vector
  pi_hat_vec <- c(pi_hat_vec, pi_hat)
}

# create df of num throws and estimates
pi_estimate <- data.frame(num_throws_ = num_throws, pi_est = pi_hat_vec)

# plot results
pi_estimate %>% ggplot(aes(x=num_throws_, y=pi_est)) + geom_point() + xlab('Number of throws') + ylab('Estimate of pi')


# Task 2

X_s <- seq(from=40, to=200, by=1)
net_wins=c()

for (X in X_s){
  
  # Initialisation
  NRepeat <- 10000 # no. of competitions entered 
  netPrize <- c(-20, -15, -5, 5, X)
  netWin <- rep(NA, NRepeat) # # net win for each competition
  set.seed(23121) # for reproducibility
  
  # Loop across the no. of competitions entered 
  for (i in seq(NRepeat)) {
    # Simulate game 1
    p <- runif(n = 1, min = 0.65, max = 0.75)
    if (rbinom(n = 1, size = 1, prob = p)) {
      # Won game 1
      # Simulate game 2
      p <- runif(n = 1, min = 0.60, max = 0.70)
      if (rbinom(n = 1, size = 1, prob = p)) {
        # Won game 2 
        # Simulate game 3
        p <- runif(n = 1, min = 0.55, max = 0.65)
        if (rbinom(n = 1, size = 1, prob = p)) {
          # Won game 3
          # Simulate game 4
          p <- runif(n = 1, min = 0.50, max = 0.60)
          if (rbinom(n = 1, size = 1, prob = p)) {
            # Won game 4
            netWin[i] <- netPrize[5]
          } else {
            # Lost game 4
            netWin[i] <- netPrize[4]
          } # end of game 4
        } else {
          # Lost game 3
          netWin[i] <- netPrize[3]
        } # end of game 3
      } else {
        # Lost game 2
        netWin[i] <- netPrize[2]
      } # end of game 2
    } else { 
      # Lost game 1
      netWin[i] <- netPrize[1]
    } # end of game 1
  } # end of loop
  
  # Compute expected net winning
  expectedNetWin <- mean(netWin)
  
  net_wins=c(net_wins, expectedNetWin)

}



# create df 
wins_estimate <- data.frame(xs = X_s, ys = net_wins)

# plot results
wins_estimate %>% ggplot(aes(x=xs,  y=ys)) + geom_point() + xlab('X') + ylab('Estimated net earnings')


# At what point do earnings exceed £15?
closest <- min(abs(wins_estimate$ys - 15)) + 15
wins_estimate[wins_estimate$ys == closest,]
# At £160 earnings exceed £15


# Task 3

# Following a similar procedure to the one described above,
# derive an equivalent randomisation test for the one-sample t-test.

# Initialisation
set.seed(29731) # for reproducibility
N <- 100 # no. of samples in each group
muX <- 10 # mean in group X
muY <- 20 # mean in group Y
std <- 3 # standard deviation for both groups

# Create the "observed" data
obsData <- data.frame(Group = c(rep("X", N), 
                                rep("Y", N)),
                      Value = c(rnorm(N, muX, std),
                                rnorm(N, muY, std)))

# Run the t.test

# Alternative one-sided
# meanX > meanY
t.test(Value ~ Group, data = obsData,
       alternative = "greater")

# Initialisation
NRepeat <- 1000 # no. of times we will shuffle the labels
simData <- obsData # initialise simulated data frame
testStat <- rep(NA, NRepeat) # store test statistic
set.seed(15134) # for reproducibility

# Loop NRepeat times
for (i in seq(NRepeat)) 
{
  # Shuffle the group labels (data stays the same)
  simData$Group <- sample(simData$Group, 2*N, replace=F)
  
  # Compute and store test statistic
  # In this case, it's the differences across groups
  meanX <- sapply(simData[simData$Group %in% "X", Value], mean)
  meanY <- sapply(simData$Value[simData$Group %in% "Y", Value], mean)
  testStat[i] <- meanX - meanY
}

# Observed test statistic
obsStat <- sapply(obsData$Value[obsData$Group %in% "X", Value], mean) - 
  sapply(obsData$Value[obsData$Group %in% "Y", Value], mean)

# Histogram
hist(testStat, col="skyblue",
     xlim = c(-abs(obsStat),
              abs(obsStat)))
abline(v = obsStat, lwd = 3)

# Compute p-values
testStat <- c(testStat, obsStat)

# One-sided: meanX > meanY
sum(testStat >= obsStat)/(NRepeat + 1)


# Task 3

# Download the dataset soil.csv from Moodle, which contains measurements of
# different elements (zinc, ytterbium and lutetium) at different sites
# (soil types). Don’t worry about what the different soil types mean for
# the purpose of this task.

# Use randomisation tests to test for differences in all element levels
# between bhb and mb soil types.

soil_df <- read_csv('soil.csv')
# H_0:  mu_bhb = mu_mb, H_1:  mu_bhb =! mu_mb

# Initialisation
NRepeat <- 1000 # no. of times we will shuffle the labels
simData <- soil_df # initialise simulated data frame
testStat <- rep(NA, NRepeat) # store test statistic
testStats <- list('zinc' = testStat,
                  'ytterbium' = testStat,
                  'lutetium' = testStat)
obsStats <- list('zinc' = NA,
                  'ytterbium' = NA,
                  'lutetium' = NA)
testStats <- list('zinc' = NA,
                 'ytterbium' = NA,
                 'lutetium' = NA)
p_values <- list('zinc' = NA,
                 'ytterbium' = NA,
                 'lutetium' = NA)

set.seed(15134) # for reproducibility
elements <- c('zinc', 'ytterbium', 'lutetium')

for (element in elements) {

  # Loop NRepeat times
  for (i in seq(NRepeat)) 
  {
    # Shuffle the group labels (data stays the same)
    simData$site <- sample(simData$site, dim(simData)[1], replace=F)
    
    
    # Compute and store test statistic
    # In this case, it's the difference across groups
    meanX <- sapply(simData[simData$site %in% "bhb",element], mean)
    meanY <- sapply(simData[simData$site %in% "mb",element], mean)
    testStats[[element]][i] <- meanX - meanY
  }
  
  # Observed test statistic
  obsStats[[element]] <- sapply(soil_df[soil_df$site %in% "bhb", element], mean) - 
    sapply(soil_df[soil_df$site %in% "mb", element], mean)
  
  # Compute p-values
  testStats[[element]] <- c(testStats[[element]], obsStats[[element]])
  
  # Two-sided
  p_values[[element]] <- sum(abs(testStats[[element]]) >= abs(obsStats[[element]]))/(NRepeat + 1)

}

testStats_zinc <- data.frame(test_stat = unlist(testStats[['zinc']]),
                         element = rep('zinc', each=NRepeat + 1))
plot_zinc <- testStats_zinc %>% ggplot(aes(x=test_stat)) + geom_histogram() + geom_vline(xintercept=obsStats[['zinc']], color = 'red')

testStats_ytterbium <- data.frame(test_stat = unlist(testStats[['ytterbium']]),
                             element = rep('ytterbium', each=NRepeat + 1))
plot_ytterbium <- testStats_ytterbium %>% ggplot(aes(x=test_stat)) + geom_histogram() + geom_vline(xintercept=obsStats[['ytterbium']], color = 'red')

testStats_lutetium <- data.frame(test_stat = unlist(testStats[['lutetium']]),
                             element = rep('lutetium', each=NRepeat + 1))
plot_lutetium <- testStats_lutetium %>% ggplot(aes(x=test_stat)) + geom_histogram() + geom_vline(xintercept=obsStats[['lutetium']], color = 'red')

library(gridExtra)
grid.arrange(plot_zinc, plot_ytterbium, plot_lutetium, ncol=2)

# All p-values seem the same? Unless I made some kind of mistake
