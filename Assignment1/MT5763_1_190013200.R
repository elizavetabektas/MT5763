
######################### Loading Packages and Data ############################

library(tidyverse)
library(lubridate) #for handling dates
library(cowplot) # for plotting graphs side-by-side

dfs <- list("Seoul" = read_csv("./BikeSeoul.csv"),
            "Washington DC" = read_csv("./BikeWashingtonDC.csv"))
# since a lot of code below is repetitive, I decided to store the dataframes
# in a list for ease

############################## Data Wrangling #################################

dfs[["Seoul"]] <- dfs[["Seoul"]] %>%
  
  # removing columns specified
  select(-c("Visibility (10m)",
            "Dew point temperature(C)",
            "Solar Radiation (MJ/m2)",
            "Rainfall(mm)",
            "Snowfall (cm)")
  ) %>%
  
  # filtering observations for which no bike count data was collected
  filter(`Functioning Day` == "Yes"
  )%>%
  
  # removing functioning day column as it is no longer required
  select(-c(`Functioning Day`)
  ) %>%
  
  # changing names of columns to be consistent with other dataframe
  rename(Count = `Rented Bike Count`,
         Temperature = `Temperature(C)`,
         Humidity = `Humidity(%)`,
         WindSpeed = `Wind speed (m/s)`,
         Season = Seasons
  ) %>%
  
  # converting date to a date object
  mutate(Date = dmy(Date)
  ) %>%
  
  mutate(FullDate = make_datetime(year(Date), # creating variable FullDate containing hour
                                  month(Date),
                                  mday(Date),
                                  Hour,
                                  tz="Asia/Seoul"),
         # changing factor levels of Holiday to Yes/No
         Holiday = factor(ifelse(Holiday == "Holiday", "Yes", "No"),
                          levels=c("Yes", "No")),
         # changing order of factor levels
         Season = factor(Season,
                         levels=c("Spring", "Summer", "Autumn", "Winter")))

dfs[["Washington DC"]] <- dfs[["Washington DC"]] %>%
  
  # removing specified columns
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
  
  # changing names of columns to match the ones for Seoul
  rename(Count = cnt,
         Temperature = temp,
         Humidity = hum,
         WindSpeed = windspeed,
         Season = season,
         Hour = hr,
         Holiday = holiday,
         Date = dteday
  ) %>%
  
  mutate(Humidity = Humidity*100,              # converting Humidity to %
         Temperature = (Temperature*(39+8)-8), # converting to degrees Celcius
         WindSpeed = WindSpeed*67*1000/3600,   #  converting to m/s
         
         # changing factor levels to match Seoul
         Season = factor(recode(Season,
                                "1"="Winter",
                                "2"="Spring",
                                "3"="Summer",
                                "4"="Autumn"),
                         levels=c("Spring", "Summer", "Autumn", "Winter")),
         
         # changing factor levels of Holiday to Yes/No
         Holiday = factor(ifelse(Holiday == 1, "Yes", "No"),
                          levels=c("Yes", "No")),
         
         # converting Date to a date object
         Date = ymd(Date)) %>%
  
  # creating FullDate so it contains hour
  mutate(FullDate = make_datetime(year(Date),
                                  month(Date),
                                  mday(Date),
                                  Hour,
                                  tz="America/New_York"))
# Washington DC and New York have same timezone

############################# Data Visualisation ###############################

# ---------------------------------------------------------------------------- #
## How does air temperature varies over the course of a year?
# ---------------------------------------------------------------------------- #

```{r air temperature variation over a year, include=FALSE}

daily_temperature_plts <- list()

for (i in names(dfs)){
  daily_temperature_plts[[i]] <- dfs[[i]] %>%
    
    # for each day, find average air temperature
    group_by(Date) %>%
    summarize(DailyTemperature = mean(Temperature)) %>%
    
    # plot points for every day and smooth line to show trend
    ggplot(mapping = aes(x=Date, y=DailyTemperature)) +
    geom_point(color=ifelse(i=="Seoul", "orchid", "darkcyan"), size=0.5) +
    geom_smooth(color="black", method = "gam") +
    scale_x_date(date_labels = "%b %Y",       # for consistency between datasets
                 breaks = ifelse(i=="Seoul", "2 months", "4 months")) +
    
    # changing default titles
    xlab("Date") +
    ylab("Temperature (°C)") +
    ggtitle(i) +
    theme(plot.title = element_text(hjust = 0.5))
}

# creating grid to display plots side-by-side
daily_temperature_plt <- plot_grid(daily_temperature_plts[["Seoul"]],
                                   daily_temperature_plts[["Washington DC"]],
                                   nrow = 2)
daily_temperature_plt

# ---------------------------------------------------------------------------- #
## Do seasons affect the average number of rented bikes?
# ---------------------------------------------------------------------------- #

bar_plts <- list()
# since a lot of the plots were similar, I decided to put them all in one list

for (i in names(dfs)){
  title <- paste(i, "seasons", sep="_")
  bar_plts[[title]] <- dfs[[i]] %>%
    
    # calculating mean bikes rented in each season and error to calculate 95% CIs
    group_by(Season) %>%
    summarise(MeanRentedBikes = mean(Count),
              error = qnorm(0.975)*sd(Count)/sqrt(length(Count))) %>%
    
    # plotting bar graph with 99% CIs
    ggplot(mapping = aes(x=Season, y=MeanRentedBikes)) +
    geom_bar(stat = "identity",
             fill = ifelse(i=="Seoul", "orchid", "darkcyan")) +
    geom_errorbar(aes(ymin=MeanRentedBikes-error,
                      ymax=MeanRentedBikes+error),
                  width=0.2) +
    geom_label(mapping = aes(label=round(MeanRentedBikes)), size=1.5) +
    
    # changing default titles
    ylab("Average Number of Rented Bikes per Hour") +
    ggtitle(i) +
    theme(plot.title = element_text(hjust = 0.5))
}

# as before, plotting above graphs side by side
seasons_plt<- plot_grid(bar_plts[["Seoul_seasons"]],
                        bar_plts[["Washington DC_seasons"]])

# ---------------------------------------------------------------------------- #
## Do holidays increase or decrease the demand for rented bikes?
# ---------------------------------------------------------------------------- #

for (i in names(dfs)){
  title <- paste(i, "holidays", sep="_")
  bar_plts[[title]] <- dfs[[i]] %>%
    
    # calculating mean bikes rented in each season and error to calculate 95% CIs
    group_by(Holiday) %>%
    summarise(MeanRentedBikes = mean(Count),
              error = qnorm(0.975)*sd(Count)/sqrt(length(Count))) %>%
    
    # plotting bar graph with 99% CIs
    ggplot(mapping = aes(x=Holiday, y=MeanRentedBikes)) +
    geom_bar(stat = "identity",
             fill = ifelse(i=="Seoul", "orchid", "darkcyan")) +
    geom_errorbar(aes(ymin=MeanRentedBikes-error,
                      ymax=MeanRentedBikes+error),
                  width=0.2) +
    geom_label(mapping = aes(label=round(MeanRentedBikes)), size=1.5) +
    
    # changing default titles
    xlab("On Holiday? Yes/No") +
    ylab("Average Number of Rented Bikes") +
    ggtitle(i) +
    theme(plot.title = element_text(hjust = 0.5))
}

# as before, plotting above graphs side by side
holidays_plt_untitled <- plot_grid(bar_plts[["Seoul_holidays"]],
                                   bar_plts[["Washington DC_holidays"]])
holidays_plt_title <- ggdraw() +
  draw_label("Effect of Holidays on Average Number of Rented Bikes",
             fontface = 'bold')
holidays_plt <- plot_grid(holidays_plt_title, holidays_plt_untitled, ncol = 1, rel_heights = c(0.1, 1))

holidays_plt_untitled

# ---------------------------------------------------------------------------- #
## How does the time of day affect the demand for rented bikes?
# ---------------------------------------------------------------------------- #

for (i in names(dfs)){
  title <- paste(i, "time_of_day", sep="_")
  bar_plts[[title]] <- dfs[[i]] %>%
    
    # calculating mean bikes rented in each season and error to calculate 95% CIs
    group_by(Hour) %>%
    summarise(MeanRentedBikes = mean(Count),
              error = qnorm(0.975)*sd(Count)/sqrt(length(Count))) %>%
    
    # plotting bar graph with 99% CIs
    ggplot(mapping = aes(x=Hour, y=MeanRentedBikes)) +
    geom_bar(stat = "identity",
             fill = ifelse(i=="Seoul", "orchid", "darkcyan")) +
    geom_errorbar(aes(ymin=MeanRentedBikes-error,
                      ymax=MeanRentedBikes+error),
                  width=0.2) +
    
    # changing default titles
    xlab("Hour") +
    ylab("Average Number of Rented Bikes") +
    ggtitle(i) +
    theme(plot.title = element_text(hjust = 0.5))
}

# as before, plotting above graphs side by side
day_time_plt <- plot_grid(bar_plts[["Seoul_time_of_day"]],
                          bar_plts[["Washington DC_time_of_day"]])
day_time_plt

# ---------------------------------------------------------------------------- #
## Is there an association between bike demand and the three meteorological
## variables (air temperature, wind speed and humidity)?
# ---------------------------------------------------------------------------- #

meteorogical_plts <- list()

for (name in names(dfs)){
  temperature_title <- paste(name, "temperature", sep="_")
  temperature_plot <- dfs[[name]] %>%
    ggplot(mapping = aes(x=Temperature, y=Count)) +
    geom_point(size=0.2) +
    geom_smooth(fill='red') +
    xlab("Air Temperature (°C)") +
    ylab("Number of Bikes Rented")
  
  wind_speed_title <- paste(name, "wind_speed", sep="_")
  wind_speed_plot <- dfs[[name]] %>%
    ggplot(mapping = aes(x=WindSpeed, y=Count)) +
    geom_point(size=0.2) +
    geom_smooth(fill='red') +
    xlab("Wind Speed (m/s)") +
    ylab("Number of Bikes Rented")
  
  humidity_title <- paste(name, "humidity", sep="_")
  humidity_plot <- dfs[[name]] %>%
    ggplot(mapping = aes(x=Humidity, y=Count)) +
    geom_point(size=0.2) +
    geom_smooth(fill='red') +
    xlab("Humidity (%)") +
    ylab("Number of Bikes Rented")
  
  untitled_plot <- plot_grid(temperature_plot,
                             wind_speed_plot,
                             humidity_plot,
                             nrow = 3)
  
  plt_title <- ggdraw() + draw_label(name)
  
  meteorogical_plts[[name]] <- plot_grid(plt_title,
                                         untitled_plot,
                                         ncol=1,
                                         rel_heights = c(0.05, 1))
}

meteorology_plt_title <- ggdraw() +
  draw_label("Effect of Meteorological Variables on Bike Demand",
             fontface = 'bold')

meteorology_plt <- plot_grid(#meteorology_plt_title,
  meteorogical_plts[["Seoul"]],
  meteorogical_plts[["Washington DC"]],
  ncol = 2,
  rel_heights = c(1, 1))

meteorology_plt

############################ Statistical Modelling ##############################

To further explore the relationship between bike demand and meteorological variables, we can fit linear models.

# ---------------------------------------------------------------------------- #
## Fitting Linear Models
# ---------------------------------------------------------------------------- #

### Seoul

Below is a summary of the fitted model for the Seoul dataset.

# fitting linear model for Seoul
seoul_lm <- lm(log(Count) ~ Season + Temperature + Humidity + WindSpeed,
data = dfs[["Seoul"]])

summary(seoul_lm) # summary of Seoul linear model

# Histogram of residuals
hist(resid(seoul_lm), main = "Histogram of Residuals", col = "orchid")

# Other useful plots
par(mfrow=c(2,2))
plot(seoul_lm)
par(mfrow=c(1,1))

### Washington DC

Below is a summary of the fitted model for the Washington DC dataset:

washington_lm <- lm(log(Count) ~ Season + Temperature + Humidity + WindSpeed,
data = dfs[["Washington DC"]])

summary(washington_lm)

# Histogram of residuals
hist(resid(washington_lm), main = "Histogram of Residuals", col="darkcyan")

# Other useful plots
par(mfrow=c(2,2))
plot(washington_lm)
par(mfrow=c(1,1))

# ---------------------------------------------------------------------------- #
# Calculating Confidence Intervals for Estimated Regression Coefficients 
# ---------------------------------------------------------------------------- #


print("Seoul")
confint(seoul_lm, level=0.97)
print("Washington DC")
confint(washington_lm, level=0.97)

# ---------------------------------------------------------------------------- #
# Do you think these confidence intervals are reliable?
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Prediction
# ---------------------------------------------------------------------------- #

# creating new dataframe with new data
new_data <- as_tibble(data.frame(Season = "Winter", 
Temperature = 0,
WindSpeed = 0.5,
Humidity = 20))

print("Seoul")
predict_seoul <- predict.lm(seoul_lm, new_data, interval="prediction", level = 0.9)
predict_seoul

print("Washington DC")
predict_washington <- predict.lm(washington_lm, new_data, interval="prediction", level = 0.9)
predict_washington
