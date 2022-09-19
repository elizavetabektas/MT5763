# load packages
library(tidyverse)

# read data into R
ff <- readRDS("ff.rds")

# summarise dataset
summary(ff)

#boxplots of thorax against group
ff %>% ggplot(aes(x=group, y=thorax)) + geom_boxplot()
  
#boxplots of longevity against group
ff %>% ggplot(aes(x=group, y=longevity)) + geom_boxplot()

#scatterplot of longevity against thorax
ff %>% ggplot(aes(x=thorax, y=longevity)) + geom_point()
