library(fable)
library(feasts)
library(readr)
library(dplyr)
library(tibble)
library(tsibble)
library(tidyr)
library(lubridate)
library(ggplot2)

rain_thresh = 0.25

rain <- read_csv("data.csv", skip = 1)

rain_ts <- tsibble(rain)

rain_mo <- rain_ts %>%
  index_by(Month = ~ yearmonth(.)) %>%
  summarise(ttl_rain = sum(`PRCP (Inches)`, na.rm = TRUE))

ggplot(rain_mo, aes(Month, ttl_rain)) +
  geom_line(color = "Purple")

rain_mo <- rain_ts |> filter(!(Date <= as.Date("2016-01-01"))) %>%
  index_by(Month = ~ yearmonth(.)) %>%
  summarise(ttl_rain = sum(`PRCP (Inches)`, na.rm = TRUE))

ggplot(rain_mo, aes(Month, ttl_rain)) +
  geom_col(color = "purple")

rain_count <- rain_ts |> filter(!(Date <= as.Date("2016-01-01"))) %>%
  index_by(Month = ~ yearmonth(.)) %>%
  summarise(rain_days = sum(`PRCP (Inches)` > rain_thresh, na.rm = TRUE))

ggplot(rain_count, aes(Month, rain_days)) +
  geom_col(color = "purple")

fit <- rain_count %>%
  model(arima_model = ARIMA(rain_days))


fc <- fit %>%
  forecast(h = "12 month")

autoplot(fc, rain_count)

rain_forecast <- fc %>%
  rename(rain_days_model = rain_days) %>%
  mutate(rain_days = .mean, type = "forecast") %>%
  as_tsibble() %>%
  select(Month, rain_days, type)

rain_count <- rain_count %>% 
  mutate(type = "historical")

bind_rows(rain_count, rain_forecast) %>%
  ggplot(aes(x = Month, y = rain_days, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c("historical" = "purple", 
                               "forecast" = "blue")) +
  labs(title = "Rain Day Forecast", 
       x = "Date", 
       y = "Days of Rain (over 0.25)")


