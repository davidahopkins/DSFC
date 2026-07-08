library(dplyr)
library(fredr)
library(ggplot2)
library(tidyr)
library(tidyverse)


#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")
PPI <- "PPIACO"


#current data date (most recent update)

start_date = "2025-01-01"
data_date = "2026-01-01"


#### Functions ####

fred_table <- function(df, id, start_date, end_date) {
  df <- fredr(
    series_id = id,
    observation_start = as.Date(start_date),
    observation_end = as.Date(end_date)
  )
}

api_data_pro <- function(df, id, df_start = start_date, df_end = data_date,
                         multiplier = 1, col1 = "col1", col2 = "col2") {
  df <- fred_table(df, id, df_start, df_end)
  df <- select(df, -c(series_id, realtime_start, realtime_end))
  df$value <- df$value * multiplier
  colnames(df) <- c(col1, col2)
  return(df)
}


#### Variables ####

cnc_index <- "WPS1333"

cnc <- api_data_pro(cnc, cnc_index, col1 = "Date", 
                         col2 = "concrete")

ggplot(cnc, aes(x = Date, y = concrete)) +
  geom_line()

stl_index <- "WPU1017"

stl <- api_data_pro(cnc, cnc_index, col1 = "Date", 
                    col2 = "steel")

ggplot(cnc, aes(x = Date, y = steel)) +
  geom_line()

cnc_chng <- cnc %>%
  summarise(result = ((last(concrete) - first(concrete)) / first(concrete)) + 1)

stl_chng <- stl %>%
  summarise(result = ((last(steel) - first(steel)) / first(steel)) + 1)











