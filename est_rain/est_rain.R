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
  model(forecast_model = ARIMA(rain_days))

forecast <- fit %>%
  forecast(h = "12 month")

autoplot(forecast, rain_count)

rain_forecast <- forecast %>%
  rename(rain_days_model = rain_days) %>%
  mutate(int_95 = hilo(rain_days_model, 95),
         int_80 = hilo(rain_days_model, 80),
         rain_days = .mean, 
         type = "Forecast") %>%
  as_tsibble() %>%
  unpack_hilo(c(int_95, int_80)) %>%
  mutate(int_95_lower = pmax(0, int_95_lower),
         int_80_lower = pmax(0, int_80_lower)) %>%
  select(Month, rain_days, type, int_95_upper,
         int_95_lower, int_80_upper, int_80_lower)

rain_count <- rain_count %>%
  mutate(int_95_upper = rain_days,
         int_95_lower = rain_days,
         int_80_upper = rain_days,
         int_80_lower = rain_days, 
         type = "Historical")

bind_rows(rain_count, rain_forecast) %>%
  ggplot(aes(x = Month, y = rain_days, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c("Historical" = "purple", 
                               "Forecast" = "blue")) +
  labs(title = "Rain Day Forecast", 
       x = "Date", 
       y = "Days w/Rain over 0.25 in") +
  theme(legend.title = element_blank())

rain_data <- bind_rows(rain_count, rain_forecast)

forecast_ttls <- rain_forecast %>%
  as_tibble() %>%
  ungroup() %>%
  summarise(ttl_mean = round(sum(rain_days), 1),
            ttl_95_up = round(sum(int_95_upper), 1),
            ttl_95_low = round(sum(int_95_lower), 1),
            ttl_80_up = round(sum(int_80_upper), 1),
            ttl_80_low = round(sum(int_80_lower), 1))

ttl_metrics <- paste0(
  "Forecast Metrics:\n",
  "Mean: ", forecast_ttls$ttl_mean, " days\n",
  "80% CI: ", forecast_ttls$ttl_80_up, " days\n",
  "95% CI: ", forecast_ttls$ttl_95_up, " days\n")

ggplot(rain_data, aes(x = Month, y = rain_days)) + 
  geom_linerange(data = filter(rain_data, type == "Forecast"), 
                 aes(ymin = int_95_lower, ymax = int_95_upper),
                   color = "skyblue", alpha = 0.4, linewidth = 2) + 
  geom_linerange(data = filter(rain_data, type == "Forecast"), 
                 aes(ymin = int_80_lower, ymax = int_80_upper), 
                     color = "steelblue", alpha = 0.5, linewidth = 2) + 
  geom_col(aes(fill = type)) +  
  geom_errorbar(data = filter(rain_data, type == "Forecast"),
              aes(ymin = int_95_lower, ymax = int_95_upper), 
              color = "darkblue", alpha = 0.8) + 
  scale_fill_manual(values = c("Historical" = "purple", 
                             "Forecast" = "blue")) + 
  annotate(
    "text",
    x = max(rain_data$Month), y = Inf,
    label = ttl_metrics,
    hjust = .85, vjust = 1.1,
    size = 4) + 
  labs(title = "Rain Day Forecast", 
     x = "Date", 
     y = "Days w/Rain over 0.25 in") + 
  theme(legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "gray95"))

