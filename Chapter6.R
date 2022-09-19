# 6.3 Introduction to ggplot2

library(tidyverse)

# Task 1
# Let’s return to the density plot example.
# Produce a plot containing three density plots for Sepal.Length,
# stratified by Species.
# Hint: use the geom_density() geom, which requires an x aesthetic only.
ggplot(iris) + geom_density(aes(x=Sepal.Length, fill=Species), alpha=0.5) + xlab('Sepal Length (cm)') + ylab('Density') + ggtitle('Density plots of sepal length by species')

# Task 2
ggplot(iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), colour = Species)
# Error because colour is not part of aesthetics, so it can't find species

# 6.4 More complex example: Gapminder
library(gapminder)
# Task 1
ggplot(gapminder[gapminder$year==1952, ], aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop)) + geom_point()

# Task 2
# part 1
ggplot(gapminder[gapminder$year==1952, ], aes(x=log10(gdpPercap), y=lifeExp, colour=continent, size=pop)) + geom_point()
# part 2
ggplot(gapminder[gapminder$year==1952, ], aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop)) + geom_point() +
  scale_x_log10()
# the plots differ in x axis

# Task 3
## remove Oceania (for exposition purposes only)
gapminderNoOc <- gapminder[gapminder$continent != "Oceania", ]

## produce overlapped density plot
ggplot(gapminderNoOc[gapminderNoOc$year == 1952, ],
       aes(x = gdpPercap, fill = continent)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  xlab("log10(GDP per capita)") + 
  ylab("Count") +
  ggtitle("1952") +
  labs(fill = "Continent")

# Task 4
#Write a function in R that takes a data and a year argument and
#plots a stacked density plot for a given year.
#Use this to plot the data for 1952, 1982, 1992 and 2002.
#Remember that if you want to plot a ggplot2 figure from inside a function,
#you will have to call print() explicitly.

yearly_plot <- function(data, year){
  plt <- ggplot(data[data$year == year, ],
         aes(x = gdpPercap, fill = continent)) +
    geom_density(position='stack') +
    scale_x_log10() +
    xlab("log10(GDP per capita)") + 
    ylab("Count") +
    ggtitle(as.character(year)) +
    labs(fill = "Continent")
  print(plt)
}
for(i in c(1952, 1982, 1992, 2002)) {
  yearly_plot(gapminderNoOc, i)
}

# Task 5

ggplot(gapminderNoOc,
       aes(x = gdpPercap, fill = continent)) +
  geom_density(position='stack') +
  scale_x_log10() +
  facet_wrap(~ year) +
  xlab("log10(GDP per capita)") + 
  ylab("Count")


# 6.5 Additional Task
ff <- readRDS("ff.rds")
ggplot(ff, aes(x=thorax, y=longevity, linetype=type, shape=type)) +
  geom_point() +
  facet_wrap(~ partners, nrow=2) +
  stat_smooth(method = "lm", se = F) +
  labs(linetype = "Partner Type", shape = "Partner Type") +
  ylab("Longevity (days)") +
  xlab("Thorax length (mm)")









