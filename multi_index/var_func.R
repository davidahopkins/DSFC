#### Packages ####

#install.packages("blsR")
#install.packages("tsibble")
#install.packages("tsibbledata")
#install.packages("feasts")
#install.packages("fable")

library(blsR)
library(tibble)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)
library(fredr)

#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")
bls_set_key("9f8572a7b9514593b2b6896c0de5a6e2")

#current data date (most recent update)

start_date = "2007-03-01"
data_date = "2024-03-01"


#pre-covid range

p1_start = "2006-03-01" 
p1_end = "2020-03-01"

#covid range

p2_start = "2020-04-01"
p2_end = "2022-06-01"

#post covid range

p3_start = "2022-07-01"
p3_end = "2024-03-01"

#Employment Variables

#All employees, construction (1939, mo, thousands of persons)

EMP_CON = "USCONS"

#job openings, construction (2001, mo, level in thousands)

JOB_OP = "JTS2300JOL"

#Not in labor force, want a job, search in previous year, available to work
#(1994, mo, thousands)

LBR_FRC = "LNU05026642"


#unemployment rate, construction, private wage and salary (2000, mo, percent)

UNEMP = "LNU04032231"

#Earnings & Wages

#Average earnings, all employees, construction (2006, mo)

AVG_EARN = "CES2000000003"

#average earning, production and non-super, construction (1947, mo)

AVG_EARN_P = "CES2000000008"

#PPI Indices 

#producer price index (1913, mo)

PPI = "PPIACO"  

#producer price index, by commodity, construction materials (1947, mo)

PPI_CM = "WPUSI012011"

#PPI building material and supplies dealers (2004, mo, index)

PPI_BMS = "PCU44414441"

#producer price index, by Industry, Plastic Material and Resin Manufacturing
#(1977, mo, index)

PPI_PRM = "PCU325211325211"

#PPI by industry, Cement and Concrete Manufacturing (2004, mo, index)

PPI_CCM = "PCU32733273"

#PPI ready-mix concrete manufacturing (1965, mo)

PPI_RMM = "PCU327320327320"

#PPI ready-mix concrete materials (1989, mo)

PPI_RMC = "WPS133301"

#PPI Metals and Metal Products (1926, mo, index)

PPI_MMP = "WPU10"

#PPI Paint and Coating Manufacturing (1984, mo, index)

PPI_PC = "PCU325510325510"

#PPI Glass and Glass Product Manufacturing (2004, mo, index)

PPI_GPM = "PCU3272132721"

#PPI Lumber and Wood Products (1926, mo, index)

PPI_LWP = "WPU08"

#Construction Spending

#Total Construction spending: total construction in US (1993, mo, millions)

TTLCONS = "TTLCONS"

#Total Construction spending: total non-residential 
#construction in US (2002, mo, millions)

TTL_NRES = "TLNRESCONS"

#Total Construction spending: total residential 
#construction in US (2002, mo, millions)

TTL_RES = "TLRESCONS"

#Total Public Construction spending: 
#total construction in US (1993, mo, millions)

TTL_PUCON = "TLPBLCONS"

#Total Private Construction spending: 
#total construction in US (1993, mo, millions)

TTL_PCON = "TLPRVCONS"

#Total Construction spending: total commercial 
#construction in US (2002, mo, millions)

TTL_COM = "TLCOMCONS"

#GDP Indices

# Gross Domestic Product (1947, mo, billions)

GDP = "GDP"  

# Real potential gross domestic product (1949, mo, billions)

RPGDP = "GDPPOT"  

#Federal Debt: Total Public Debt as Percent of GDP (1966, mo)

FDGDP = "GFDEGDQ188S" 

#CPI Indices

#CPI for All Urban Consumers: All US Cities (1947, mo, index)

CPI = "CPIAUCSL"  

#CPI for All Urban Consumers: All US Cities less Food & Energy 
#(1957, mo, index)

CCPI = "CPILFESL"  

#Month Matrix for ENR manipulations

month_matrix <- data.frame(mo_index = c(1:12), 
                           mo_abb = c("JAN", "FEB", "MAR", 
                                      "APR", "MAY", "JUN", 
                                      "JUL", "AUG", "SEP", 
                                      "OCT", "NOV", "DEC")) 

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

#lab_series <- get_series_table("LNS14000000", 
#                               start_year = 1991,
#                               end_year = 2000,
#                               parse_values = TRUE)

format_enr <- function(df){
  df <- df[, 1:13]
  df <- melt(df, id.vars = "YEAR")
  
  names(df)[2] <- "mo"
  names(df)[3] <- "index"
  df$day <- "1"
  
  df <- full_join(df, month_matrix, join_by(x$mo == y$mo_abb))
  
  df$date <- paste(df$mo_index, df$day, df$YEAR, sep = "-")
  
  df$date <- as.Date(df$date, format = "%m-%d-%Y")
  df <- df %>% select(-one_of('YEAR', 'mo', 'day', 'mo_index'))
  df <- df[order(df$date),]
}

period_select <- function(df_1, df_2, start, end) {
  df_2 <- df_1
  df_2 <- df_2[df_2$date >= start & df_2$date <= end, ]
}

single_plot <- function(df, xvar, yvar, title, xlab, ylab) {
  ggplot(data = df, aes(x = xvar, y = yvar)) + 
    geom_line(color = "blue", linewidth = 0.5) + 
    labs(title = title, x = xlab, y = ylab)
}

double_plot <- function(df, xvar1, yvar1, yvar2, title, labx, laby) {
  ggplot() +
    geom_line(data = df, aes(x = xvar1, y = yvar1), color = 'blue') +
    geom_line(data = df, aes(x = xvar1, y = yvar2), color = 'purple') +
    labs(title = title, x = labx, y = laby)
}

triple_plot <- function(df, xvar1, yvar1, yvar2, yvar3, title, labx, laby) {
  ggplot() +
    geom_line(data = df, aes(x = xvar1, y = yvar1), color = 'blue') +
    geom_line(data = df, aes(x = xvar1, y = yvar2), color = 'purple') +
    geom_line(data = df, aes(x = xvar1, y = yvar3), color = 'cyan') +
    labs(title = title, x = labx, y = laby)
}
