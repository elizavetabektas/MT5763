library(tidyverse)

seoul2 <- read_csv("./BikeSeoul.csv")
washington2 <- read_csv("./BikeWashingtonDC.csv")

########################### Data Wrangling #####################################

########################### BikeSeoul.csv #####################################


# Remove the following columns: visibility, dew point temperature,
# solar radiation, rainfall and snowfall

seoul <- seoul %>% select(-c("Visibility (10m)", "Dew point temperature(C)", "Solar Radiation (MJ/m2)", "Rainfall(mm)" , "Snowfall (cm)"))
# is there a difference betwen " and ` for column names?

#Filter out observations for which no bike count data was collected, then remove
#the functioning day column as it is no longer required.

seoul <- seoul %>% filter(`Functioning Day` == "Yes") %>% select(-c(`Functioning Day`))

# rename columns
seoul <- seoul %>% rename(Count = `Rented Bike Count`, Temperature = `Temperature(C)`,
                          Humidity = `Humidity(%)`, WindSpeed = `Wind speed (m/s)`,
                          Season = Seasons)


# convert date to a date object
library(lubridate)

seoul <- seoul %>% mutate(Date = dmy(Date))


# create a new variable called FullDate which includes the hour in it
# set minute and second to zero

seoul <- seoul %>% mutate(FullDate = make_datetime(year(Date), month(Date), mday(Date), Hour, min=0, sec=0))

# make_datetime puts a timezone, which is not wanted in model answer
seoul <- seoul %>% mutate(FullDate = strftime(FullDate, tz=""))

# Change the factor levels of Holiday to Yes/No (use this order)
seoul <- seoul %>% mutate(Holiday = ifelse(Holiday == "Holiday", "Yes", "No"))
seoul$Holiday <- factor(seoul$Holiday, levels=c("Yes", "No"))
# Change the order of the Season factor levels to Spring, Summer, Autumn and Winter
seoul$Season <- factor(seoul$Season, levels=c("Spring", "Summer", "Autumn", "Winter"))

########################### BikeWashingtonDC.csv #####################################

#Remove the following columns: unique record index, year, month, day of the week,
# working day, weather
#condition, normalised feeling temperature and number of bikes rented by casual
# and registered users
# (i.e. keep only the total count).

washington <- read_csv("./BikeWashingtonDC.csv")

washington <- washington %>% select(-c("instant","yr", "mnth", "weekday", "workingday", "weathersit", "atemp", "casual", "registered")) 
# we tell R something is an integer if we put an L at the end of the number

washington <- washington %>% rename(Count = cnt, Temperature = temp,
                          Humidity = hum, WindSpeed = windspeed,
                          Season = season, Hour = hr, Holiday = holiday, Date = dteday)

washington <- washington %>% mutate(Humidity = Humidity*100)

t_min <- -8
t_max <- 39
washington <- washington %>% mutate(Temperature = (Temperature*(t_max-t_min)+t_min))

washington <- washington %>% mutate(WindSpeed = WindSpeed*67*1000/3600)

washington <- washington %>% mutate(Holiday = ifelse(Holiday == 1, "Yes", "No"))
washington <- washington %>% mutate(Season = recode(Season, "1"="Winter", "2"="Spring", "3"="Summer", "4"="Autumn"))
washington$Holiday <- factor(washington$Holiday, levels=c("Yes", "No"))
# Change the order of the Season factor levels to Spring, Summer, Autumn and Winter
washington$Season <- factor(washington$Season, levels=c("Spring", "Summer", "Autumn", "Winter"))


# Convert Date to a date object
washington <- washington %>% mutate(Date = ymd(Date))

washington <- washington %>% mutate(FullDate = make_datetime(year(Date), month(Date), mday(Date), Hour, min=0, sec=0))

# make_datetime puts a timezone, which is not wanted in model answer
washington <- washington %>% mutate(FullDate = strftime(FullDate, tz=""))


########################### Data Visualisation #####################################

#explore (visually) the associations between bike usage, weather, time of day and holidays for both the
#Seoul and Washington datasets
#Produce any number of relevant plots to answer the following questions, and
#comment on the similarities / differences between Seoul and Washington.

######### How does air temperature varies over the course of a year? #############

# Combining datasets
seoul$City <- "Seoul"
washington$City <- "WashingtonDC"
combined_df <- full_join(seoul, washington)
combined_df <- combined_df %>% mutate(City = factor(City, levels=c("Seoul", "WashingtonDC")))

# Seoul
# It looks like Seoul has 1 year's worth of data
print(c(min(seoul$Date), max(seoul$Date)))
seoul_temp_time <- seoul %>%
  group_by(Date) %>%
  summarize(DailyTemperature = mean(Temperature)) %>%
  ggplot(mapping = aes(x=Date, y=DailyTemperature)) +
  geom_point(color="orchid") +
  geom_smooth(color="black", method = "gam") +
  xlab("Date") +
  ylab("Temperature (C)") +
  ggtitle("Temperature Over Time in Seoul")
seoul_temp_time

# Washignton DC
washington_temp_time <- washington %>%
  group_by(Date) %>%
  summarize(DailyTemperature = mean(Temperature)) %>%
  ggplot(mapping = aes(x=Date, y=DailyTemperature)) +
  geom_point(color="darkcyan") +
  geom_smooth(color = "black", method = "gam") +
  xlab("Date") +
  ylab("Temperature (C)") +
  ggtitle("Temperature Over Time in Washington DC")
washington_temp_time

# This plots 2 years worth of data
# To make this graph more comparable to Seoul's, let's average the years so we only plot 1 year

############ Do seasons affect the average number of rented bikes? ################

seoul_seasons_bikes <- seoul %>%
  group_by(Season) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Season, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Seasons on Average Number of Rented Bikes in Seoul")
seoul_seasons_bikes

washington_seasons_bikes <- washington %>%
  group_by(Season) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Season, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Seasons on Average Number of Rented Bikes in Washington DC")
washington_seasons_bikes

########## Do holidays increase or decrease the demand for rented bikes? #######

seoul_holidays <- seoul %>%
  group_by(Holiday) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Holiday, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Holidays on Average Number of Rented Bikes in Seoul")
seoul_holidays

washington_holidays <- washington %>%
  group_by(Holiday) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Holiday, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Holidays on Average Number of Rented Bikes in Washington DC")
washington_holidays


########### How does the time of day affect the demand for rented bikes? #########

seoul_day_time <- seoul %>%
  group_by(Hour) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Hour, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Time of Day on Average Number of Rented Bikes in Seoul")
seoul_day_time

washington_day_time <- washington %>%
  group_by(Hour) %>%
  summarise(AverageRentedBikes = mean(Count)) %>%
  ggplot() +
  geom_bar(mapping = aes(x=Hour, y=AverageRentedBikes), stat = "identity") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Effect of Time of Day on Average Number of Rented Bikes in Washington DC")
washington_day_time

####### Is there an association between bike demand and the three meteorological
#########variables (air temperature,wind speed and humidity)? ######################
library(gridExtra)

seoul_air_temp <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Temperature, y=Count))
seoul_wind_speed <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=WindSpeed, y=Count))
seoul_humidity <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Humidity, y=Count))
grid.arrange(seoul_air_temp, seoul_wind_speed, seoul_humidity)

washington_air_temp <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Temperature, y=Count))
washington_wind_speed <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=WindSpeed, y=Count))
washington_humidity <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Humidity, y=Count))
grid.arrange(washington_air_temp, washington_wind_speed, washington_humidity)

############################ Statistical Modelling ##############################

# For both the Seoul and Washington datasets do the following:

# Fit a linear model with log count as outcome, and season, air temperature, humidity and wind speed as
#predictors. Print out a summary of the fitted models, comment on the results and compare across the
#two cities.

# Display the 97% confidence intervals for the estimated regression coefficients. Do you think these
#confidence intervals are reliable?

# Assuming the model is trustworthy, what’s the expected number of rented bikes in winter when the air
#temperature is freezing (0◦C), in the presence of light wind (0.5m/s) and a humidity of 20%. Provide
#the 90% prediction intervals and comment on the results. Hint: Use the interval argument of the
#predict function.



