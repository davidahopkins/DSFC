#### Packages & Global####

#install.packages("tsibble")
#install.packages("tsibbledata")
#install.packages("feasts")
#install.packages("fable")

library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(feasts)
library(fable)
library(fredr)

#### weather data processing ####

precip <- read_csv("noaa_data.csv")
head(precip)

ggplot(precip, aes(x = DATE, y = PRCP)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Rainfall")

precip$day <- format(precip$DATE, format = "%b %d")
precip$month <- month(ymd(precip$DATE))
precip$year <- year(ymd(precip$DATE))
head(precip)

precip_m <- precip %>% 
  group_by(day) %>%
  summarize(mean_rain = mean(PRCP, na.rm = TRUE)) %>%
  filter(mean_rain > 0.25)

ggplot(precip_m, aes(x = day, y = mean_rain)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Rainfall")

precip_d <- precip %>% 
  filter(PRCP > 0.125) %>%
  group_by(month) %>%
  summarize(rain_days = n() / 6)

ggplot(precip_d, aes(x = month, y = rain_days) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Days w/Rainfall > 0.125")

#### weather regression ####

#### weather adjustments ####
