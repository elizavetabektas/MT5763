library(tidyverse)
library(lubridate)
library(cowplot)

seoul <- read_csv("./BikeSeoul.csv")
washington <- read_csv("./BikeWashingtonDC.csv")

########################### Data Wrangling #####################################

########################### BikeSeoul.csv #####################################

# Remove the following columns: visibility, dew point temperature,
# solar radiation, rainfall and snowfall

seoul <- seoul %>%
  select(-c("Visibility (10m)",
            "Dew point temperature(C)",
            "Solar Radiation (MJ/m2)",
            "Rainfall(mm)",
            "Snowfall (cm)")
         ) %>%
  filter(`Functioning Day` == "Yes"
         )%>%
  select(-c(`Functioning Day`)
         ) %>%
  rename(Count = `Rented Bike Count`,
         Temperature = `Temperature(C)`,
         Humidity = `Humidity(%)`,
         WindSpeed = `Wind speed (m/s)`,
         Season = Seasons
         ) %>%
  mutate(Date = dmy(Date)
         ) %>%
  mutate(FullDate = make_datetime(year(Date),
                                  month(Date),
                                  mday(Date),
                                  Hour,
                                  min=0,
                                  sec=0,
                                  tz="Asia/Seoul"),
         Holiday = factor(ifelse(Holiday == "Holiday", "Yes", "No"),
                          levels=c("Yes", "No")),
         Season = factor(Season,
                         levels=c("Spring", "Summer", "Autumn", "Winter")))

########################### BikeWashingtonDC.csv #####################################

washington <- washington %>%
  select(-c("instant",
            "yr",
            "mnth",
            "weekday",
            "workingday",
            "weathersit",
            "atemp",
            "casual",
            "registered")
         ) %>%
  rename(Count = cnt,
         Temperature = temp,
         Humidity = hum,
         WindSpeed = windspeed,
         Season = season,
         Hour = hr,
         Holiday = holiday,
         Date = dteday
         ) %>%
  mutate(Humidity = Humidity*100,
         Temperature = (Temperature*(39+8)-8),
         WindSpeed = WindSpeed*67*1000/3600,
         Season = factor(recode(Season,
                         "1"="Winter",
                         "2"="Spring",
                         "3"="Summer",
                         "4"="Autumn"),
                         levels=c("Spring", "Summer", "Autumn", "Winter")),
         Holiday = factor(ifelse(Holiday == 1, "Yes", "No"),
                          levels=c("Yes", "No")),
         Date = ymd(Date)) %>%
  mutate(FullDate = make_datetime(year(Date),
                                  month(Date),
                                  mday(Date),
                                  Hour,
                                  min=0,
                                  sec=0,
                                  tz="America/New_York")
                                  # Washington DC and New York have same timezone
         )

########################### Data Visualisation #####################################

######### How does air temperature varies over the course of a year? #############

# For hour of every day, we have the temperature
# This means there will be variation in temperature in each 24-hour period
# due to night-day fluctuations
# Let's find DailyTemperature before plotting it

seoul_daily_temperature_plt <- seoul %>%
  group_by(Date) %>%
  summarize(DailyTemperature = mean(Temperature)) %>%
  ggplot(mapping = aes(x=Date, y=DailyTemperature)) +
  geom_point(color="orchid") +
  geom_smooth(color="black", method = "gam") +
  scale_x_date(date_labels = "%b %Y") +
  xlab("Date") +
  ylab("Temperature (C)") +
  ggtitle("Seoul") +
  theme(plot.title = element_text(hjust = 0.5))

washington_daily_temperature_plt <- washington %>%
  group_by(Date) %>%
  summarize(DailyTemperature = mean(Temperature)) %>%
  ggplot(mapping = aes(x=Date, y=DailyTemperature)) +
  geom_point(color="darkcyan") +
  geom_smooth(color = "black", method = "gam") +
  scale_x_date(date_labels = "%b %Y") +
  xlab("Date") +
  ylab("Temperature (C)") +
  ggtitle("Washington DC") +
  theme(plot.title = element_text(hjust = 0.5))

daily_temperature_plt <- plot_grid(seoul_daily_temperature_plt, washington_daily_temperature_plt)

daily_temperature_plt_title <- ggdraw() +
  draw_label("Fluctuations in Daily Temperature",
             fontface = 'bold')

plot_grid(daily_temperature_plt_title, daily_temperature_plt, ncol = 1, rel_heights = c(0.1, 1))

############ Do seasons affect the average number of rented bikes? ################

seoul_seasons_plt <- seoul %>%
  group_by(Season) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Season, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
           fill = "orchid") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Seoul") +
  theme(plot.title = element_text(hjust = 0.5))
# how variable is the mean (tight CI)
# comparing means of seasons - how certain are we that they are different

washington_seasons_plt <- washington %>%
  group_by(Season) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Season, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
           fill = "darkcyan") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Washington DC") +
  theme(plot.title = element_text(hjust = 0.5))

seasons_plt <- plot_grid(seoul_seasons_plt, washington_seasons_plt)

seasons_plt_title <- ggdraw() +
  draw_label("Effect of Seasons on Bike Renting",
             fontface = 'bold')

plot_grid(seasons_plt_title, seasons_plt, ncol = 1, rel_heights = c(0.1, 1))

########## Do holidays increase or decrease the demand for rented bikes? #######

seoul_holidays_plt <- seoul %>%
  group_by(Holiday) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Holiday, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
           fill = "orchid") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  xlab("On Holiday? Yes/No") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Seoul") +
  theme(plot.title = element_text(hjust = 0.5))

washington_holidays_plt <- washington %>%
  group_by(Holiday) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Holiday, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
           fill = "darkcyan") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  xlab("On Holiday? Yes/No") +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Washington DC") +
  theme(plot.title = element_text(hjust = 0.5))

holidays_plt <- plot_grid(seoul_holidays_plt, washington_holidays_plt)

holidays_plt_title <- ggdraw() +
  draw_label("Effect of Holidays on Average Number of Rented Bikes",
             fontface = 'bold')

plot_grid(holidays_plt_title, holidays_plt, ncol = 1, rel_heights = c(0.1, 1))

########### How does the time of day affect the demand for rented bikes? #########

seoul_day_time_plt <- seoul %>%
  group_by(Hour) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Hour, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
           fill = "orchid") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Seoul") +
  theme(plot.title = element_text(hjust = 0.5))

washington_day_time_plt <- washington %>%
  group_by(Hour) %>%
  summarise(MeanRentedBikes = mean(Count),
            error = qnorm(0.995)*sd(Count)/sqrt(length(Count))) %>%
  ggplot(mapping = aes(x=Hour, y=MeanRentedBikes)) +
  geom_bar(stat = "identity",
                 fill = "darkcyan") +
  geom_errorbar(aes(ymin=MeanRentedBikes-error,
                    ymax=MeanRentedBikes+error),
                width=0.2) +
  ylab("Average Number of Rented Bikes") +
  ggtitle("Washington DC") +
  theme(plot.title = element_text(hjust = 0.5))

day_time_plt <- plot_grid(seoul_day_time_plt, washington_day_time_plt)

day_time_plt_title <- ggdraw() +
  draw_label("Effect of Holidays on Average Number of Rented Bikes",
             fontface = 'bold')

plot_grid(day_time_plt_title, day_time_plt, ncol = 1, rel_heights = c(0.1, 1))

####### Is there an association between bike demand and the three meteorological
#########variables (air temperature,wind speed and humidity)? ######################

seoul_air_temp_plt <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Temperature, y=Count)) +
  xlab("Air Temperature (C)") +
  ylab("Number of Bikes Rented")
seoul_wind_speed_plt <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=WindSpeed, y=Count)) +
  xlab("Wind Speed (m/s)") +
  ylab("Number of Bikes Rented")
seoul_humidity_plt <- seoul %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Humidity, y=Count)) +
  xlab("Humidity (%)") +
  ylab("Number of Bikes Rented")

washington_air_temp_plt <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Temperature, y=Count)) +
  xlab("Air Temperature (C)") +
  ylab("Number of Bikes Rented")
washington_wind_speed_plt <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=WindSpeed, y=Count)) +
  xlab("Wind Speed (m/s)") +
  ylab("Number of Bikes Rented")
washington_humidity_plt <- washington %>%
  ggplot() +
  geom_smooth(mapping = aes(x=Humidity, y=Count)) +
  xlab("Humidity (%)") +
  ylab("Number of Bikes Rented")

seoul_meteorology_plt_untitled <- plot_grid(seoul_air_temp_plt,
                             seoul_wind_speed_plt,
                             seoul_humidity_plt,
                             ncol = 3)
seoul_meteorology_plt_title <- ggdraw() + draw_label("Seoul")
seoul_meteorology_plt <- plot_grid(seoul_meteorology_plt_title,
                                       seoul_meteorology_plt_untitled,
                                       ncol=1,
                                       rel_heights = c(0.1, 1))

washington_meteorology_plt_untitled <- plot_grid(washington_air_temp_plt,
                             washington_wind_speed_plt,
                             washington_humidity_plt,
                             ncol = 3)
washington_meteorology_plt_title <- ggdraw() + draw_label("Washington DC")
washington_meteorology_plt <- plot_grid(washington_meteorology_plt_title,
                                       washington_meteorology_plt_untitled,
                                       ncol=1,
                                       rel_heights = c(0.1, 1))

meteorology_plt_title <- ggdraw() +
  draw_label("Effect of Meteorological Variables on Bike Demand",
             fontface = 'bold')

plot_grid(meteorology_plt_title,
          washington_meteorology_plt,
          seoul_meteorology_plt,
          ncol = 1,
          rel_heights = c(0.1, 1, 1))



############################ Statistical Modelling ##############################

# For both the Seoul and Washington datasets do the following:

# Fit a linear model with log count as outcome, and season, air temperature, humidity and wind speed as
#predictors. Print out a summary of the fitted models, comment on the results and compare across the
#two cities.

seoul_lm <- lm(log(Count) ~ Season + Temperature + Humidity + WindSpeed,
               data = seoul)
summary(seoul_lm)
# Season Autumn, Winter, Temperature, Humidity and WindSpeed are significant to the model
# we can see this from the ** or *** next to p-values
# Multiple R-Squared is 0.4941, which is not close to 1, so this model does not explain 
# variation in the data well

seoul_resid <- resid(seoul_lm)
# Histogram of residuals
hist(seoul_resid)
# Skewed left, so greater variation in left tail
# Quantile-quantile plot
qqnorm(seoul_resid)
# Normality questionable from Q-Q plot and histogram. There is more variation towards left

# Residuals vs fitted model
plot(seoul_lm)
# The red line is showing a systematic pattern for larger fitted values, 
# suggesting we cannot assume linearity 
# The red-line in the scalelocation plot should be horizontal. In this example the variance appears to be increasing
# with fitted value. Constant variance appears to not be valid.

# model doesn't fit the data well, and we can't assume normality, linearity or constant variance

######## RELEVEL to change "baseline" for linear model

# residuals are the differences between data and fitted values
# The p-value, in association with the t-statistic, help us to understand how significant our coefficient is to the model
# In practice, any p-value below 0.05 is usually deemed as significant
# It means we are confident that the coefficient is not zero, meaning the coefficient
# does in fact add value to the model by helping to explain the variance within our dependent variable
# The coefficient codes give us a quick way to visually see which coefficients are significant to the model.
# To the right of the p-values you’ll see several asterisks (or none if the coefficient is not significant to the model)
# The number of asterisks corresponds with the significance of the coefficient as described in the legend just under the coefficients section.
# The more asterisks, the more significant.

washington_lm <- lm(log(Count) ~ Season + Temperature + Humidity + WindSpeed,
               data = washington)
summary(washington_lm)
# All coefficients are statistically significant, summer is significant, unlike in Seoul
# Multiple R-squared is 0.278, which is really low
# so this model captures less variation that in seoul

washington_resid <- resid(washington_lm)
# Histogram of residuals
hist(washington_resid)
# Skewed left, so greater variation in left tail
# Quantile-quantile plot
qqnorm(washington_resid)
# Normality questionable from Q-Q plot and histogram. There is more variation towards left

# Residuals vs fitted model
plot(washington_lm)
# the plot of residuals against fitted values should show no obvious systematic pattern
#either horizontally or vertically. The residuals should be well scattered above and below
#zero, with vertical spread (indicating variance) which does not depend much on the fitted
#value
# There is a clear pattern, with red line going down and spread of points changes at larger fitted values
# normality and linearity cannot be assumed
# The red line in scale-location should be horizontal, and it is not, 
# variance is decreasing with fitted value, so constant variance is not valid

# Washington model is worse, which can be seen from R2 and graphically

# Display the 97% confidence intervals for the estimated regression coefficients.
# Do you think these confidence intervals are reliable?
confint(seoul_lm, level=0.97)
confint(washington_lm, level=0.97)
# No, because the models we fit don't capture most of the variation in the data
#IF IT OBEYS ASSUMPTIONS, then it will be reliable(trust confidence intervals are appropriate for data I have),
# may not be informative, but given data

# Assuming the model is trustworthy, what’s the expected number of rented bikes in winter when the air
#temperature is freezing (0◦C), in the presence of light wind (0.5m/s) and a humidity of 20%. Provide
#the 90% prediction intervals and comment on the results. Hint: Use the interval argument of the
#predict function.

new_data <- as_tibble(data.frame(Season = "Winter",
                                Temperature = 0,
                                WindSpeed = 0.5,
                                Humidity = 20))

predict.lm(seoul_lm, new_data, interval="prediction", level = 0.9)
# the point estimate is 5.913404
# the 97% CI is 4.290184 7.536624

predict.lm(washington_lm, new_data, interval="prediction", level = 0.9)
# the point estimate is 4.276413
# the 97% CI is 1.799728 6.753099


#################################################################### Extra code

# Combining datasets
seoul$City <- "Seoul"
washington$City <- "WashingtonDC"
combined_df <- full_join(seoul, washington) %>%
  mutate(City = factor(City, levels=c("Seoul", "WashingtonDC")))


