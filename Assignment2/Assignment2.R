#Task: Monte Carlo simulation (R)
#Write efficient R code (use parallel computation techniques where applicable)
# to solve the following problems using Monte Carlo simulation.
#Problem A
#Consider the following independent random variables:

#X∼N(μ=4,σ2=10)
#Y∼Y(a=2,b=8)
#Compute the probability that X>Y, i.e. Pr(X>Y).
#Use bootstrapping to derive the sampling distribution for your estimate of Pr(X>Y).

#Show how the sample variance of this sampling distribution changes as a function of the number of Monte Carlo simulations.

#Problem B
#Consider the following football tournament format: a team keeps playing until they accrue 7 wins or 3 losses (whichever comes first - no draws allowed). Assume a fixed win rate p∈[0,1] across all rounds (they are paired at random).

#Plot how the total number of matches played (i.e. wins + losses) varies as a function of p.

#Comment on the observed win rate relative to the assumed win rate p (i.e. if a team obtains 2 wins - 3 losses, the maximum likelihood point estimate for their win rate is 40%). Specifically, focus on the effect driven by the format of this tournament.

#Deliverables
#Include the code used to perform the simulations / calculations.

#Show, interpret and discuss the results obtained.