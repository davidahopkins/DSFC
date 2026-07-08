library(dplyr)
library(fredr)
library(ggplot2)
library(lubridate)
library(tibble)
library(tidyr)
library(tidyverse)

#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")

#current data date (most recent update)

start_date = "2025-01-01"
data_date = "2026-01-01"
t_per = time_length(start_date %--% data_date, unit = "days")

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

stl_index <- "WPU1017"

lmb_index <- "WPU08"

pnt_index <- "WPU0622"

dry_index <- "WPU13710102"

ppi_index <- "PPIACO"

#### Tables ####

cnc <- api_data_pro(cnc, cnc_index, col1 = "Date", 
                    col2 = "concrete")

stl <- api_data_pro(stl, stl_index, col1 = "Date", 
                    col2 = "steel")

lmb <- api_data_pro(lmb, lmb_index, col1 = "Date", 
                    col2 = "lumber")

dry <- api_data_pro(dry, dry_index, col1 = "Date", 
                    col2 = "drywall")

pnt <- api_data_pro(pnt, pnt_index, col1 = "Date", 
                    col2 = "paint")

ppi <- api_data_pro(ppi, ppi_index, col1 = "Date", 
                    col2 = "ppi")

#### Plots ####

ggplot(cnc, aes(x = Date, y = concrete)) +
  geom_line()

ggplot(stl, aes(x = Date, y = steel)) +
  geom_line()

ggplot(lmb, aes(x = Date, y = lumber)) +
  geom_line()

ggplot(dry, aes(x = Date, y = drywall)) +
  geom_line()

ggplot(pnt, aes(x = Date, y = paint)) +
  geom_line()

ggplot(ppi, aes(x = Date, y = ppi)) +
  geom_line()

#### Changes ####

index_chng <- data.frame(index = character(0), result = numeric(0))

index_chng <- index_chng %>%
  add_row(index = c("CNC", "STL", "LMB", "DRY", "PNT", "PPI"),
          result = c(
            cnc %>% summarise(result = ((last(concrete) - first(concrete)) / first(concrete))) %>% pull(result), 
            stl %>% summarise(result = ((last(steel) - first(steel)) / first(steel))) %>% pull(result),
            lmb %>% summarise(result = ((last(lumber) - first(lumber)) / first(lumber))) %>% pull(result),
            dry %>% summarise(result = ((last(drywall) - first(drywall)) / first(drywall))) %>% pull(result),
            pnt %>% summarise(result = ((last(paint) - first(paint)) / first(paint))) %>% pull(result),
            ppi %>% summarise(result = ((last(ppi) - first(ppi)) / first(ppi))) %>% pull(result)
            )
          )










