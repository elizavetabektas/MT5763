# 7.3 Grouping and summarising
# Task 1
# Produce a table of estimates for the mean and variance of both sepal lengths and widths, within each species.
iris %>% 
  group_by(Species) %>%
  summarise(mean_sepal_length = mean(Sepal.Length), variance_sepal_length = sd(Sepal.Length)^2,
            mean_sepal_width = mean(Sepal.Width), variance_sepal_width = sd(Sepal.Width))

# 7.4 Reshaping datasets

# Task 1
library(tidyverse)
library(gapminder)
gp_income <- read_csv("gdp_per_capita_yearly_growth.csv")
# can't read in csv, even when downloaded from website

# Task 2
ff <- readRDS("gp_hiv.rds")
# this task is already done by reading in the dataset

# Task 3
# Produce a line plot of HIV prevalence over time for Uganda and Brazil.
# Use different colours for the different countries. Use ggplot2,
# and notice that the first argument to the ggplot() function is a data.frame!
# You might also have to convert the year column to a numeric.
ff %>% filter(country %in% c('Uganda', 'Brazil')) %>%
  ggplot(aes(x=as.numeric(year), y=prevalence, colour=country)) + geom_line() + xlab("Year") + ylab("Prevalence") +
  labs(colour = "Country")

# 7.5 Joins
# Can't do tasks as un-loadable dataset is required

# 7.6 Comprehensive Example
## read German data in
germany <- read_csv("Germanypop.csv")

## read Mexican data in
mexico <- read_csv("Mexicopop.csv")

## read US data in
us <- read_csv("USpop.csv")

