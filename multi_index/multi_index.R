library(fredr)
library(tidyr)
library(tidyverse)


#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")
PPI -> "PPIACO"


#current data date (most recent update)

start_date = "1993-01-01"
data_date = "2025-01-01"


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