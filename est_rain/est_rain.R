library(readr)
library(dplyr)
library(tibble)
library(tsibble)
library(tidyr)
library(lubridate)
library(ggplot2)

rain <- read_csv("data.csv", skip = 1)

rain_ts <- tsibble(rain)

rain_mo <- rain_ts %>%
  index_by(Month = ~ yearmonth(.)) %>%
  summarise()