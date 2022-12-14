# 12 Data Acquisition

# 12.1 Web Scraping

## Part 1

# to check if we can access the data on the website
library(robotstxt)
paths_allowed("https://uk.investing.com/") # aye
paths_allowed("https://www.facebook.com/") # a big no

# Load required libraries
library(rvest)      # for web scraping 
library(stringr)    # to perform string operations
library(lubridate)  # to change strings to dates

# Web page containing bitcoin data 
URL <- "https://www.imdb.com/chart/top/"

# Download the whole web page
htmlPage <- read_html(URL) 

# Movie title column (as a character vector)
movie_title <- htmlPage %>% 
  html_nodes(".titleColumn a") %>%
  html_text() 

# Release year (as a numeric vector)
release_year <- htmlPage %>% 
  html_nodes(".secondaryInfo") %>%
  html_text() %>% 
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  as.numeric()

#IMDb rating (as a numeric vector)
imdb_rating <- htmlPage %>% 
  html_nodes("strong") %>%
  html_text() %>% 
  as.numeric()

# Combine into a data frame
df <- data.frame(movie_title, release_year, imdb_rating)
head(df)

## Part 2
library(tidyverse)
df %>% group_by(release_year) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  head(15)

## Part 3
library(ggplot2)
# Plotting yearly average rating
yearly_averages <- df %>%
  group_by(release_year) %>%
  summarise(average = mean(imdb_rating)) %>%
  arrange(release_year)

yearly_plot <- ggplot(yearly_averages, aes(x=release_year, y=average)) +
  geom_point() + 
  stat_smooth(method = "lm") 
yearly_plot

# 12.2 APIs

# Load required libraries
library(lubridate)
library(httr)
library(jsonlite)
library(tidyverse)

# API call
URL <- "https://api.coindesk.com/v1/bpi/historical/close.json?start=2020-03-01&end=2020-03-31"

# Send http request
response <- httr::GET(URL)

# Read / unpack a binary file
dtaJSON <- httr::content(response, as = "text", encoding = "UTF-8")

# Parse JSON file
dta <- jsonlite::fromJSON(dtaJSON)

# Extract the relevant bits of data into a data frame
df2 <- data.frame(Price=unlist(dta$bpi), 
                 Date=names(dta$bpi))
rownames(df2) <- NULL # remove row names
df2$Date <- lubridate::as_date(df2$Date) # convert to Date

# Plot data
ggplot(df2, aes(x = Date, y = Price)) +
  geom_point() +
  geom_line() +
  ggtitle("Bitcoin price index")










