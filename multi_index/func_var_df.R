#### Libraries ####

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
library(dplyr)
library(ggfortify)
library(reshape2)
library(patchwork)
library(zoo)

#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")
bls_set_key("9f8572a7b9514593b2b6896c0de5a6e2")

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

min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x)) *100
}

log_scale <- function(x){
  log10(x)
}

sqr_scale <- function(x){
  sqrt(x)
}

cubr_scale <- function(x){
  x^1/3
}
#### Plotting settings ####

bci_color <- "aquamarine3"
cci_color <- "blueviolet"
sli_color <- "coral1"
cli_color <- "blue3"
mpi_color <- "deeppink1"
fed_min_color <- "aquamarine3"
cal_min_color <- "darkviolet"
const_ns_color <- "deeppink1"
lumber_color <- "aquamarine3"
paint_color <- "darkviolet"
steel_color <- "coral1"
concrete_color <- "blue3"
ttl_cnst_color <- "darkgreen"
forecast_color <- "deeppink4"
plot_line_color = "lightgray"
plot_color = "white"
plot_line_type = "dotted"
plot_line = 0.25

data_line = 0.5
prime_line = 1




enr_colors <- c("BCI" = bci_color, "CCI" = cci_color, 
                      "SLI" = sli_color, "CLI" = cli_color, 
                      "MPI" = mpi_color)

wage_colors <- c("SLI" = sli_color, "CLI" = cli_color, 
                 "fed_min_wage" = fed_min_color, 
                 "cal_min_wage" = cal_min_color, 
                 "const_ns_wage" = const_ns_color)

mat_colors = c("MPI" = mpi_color, 
               "lumber" = lumber_color,
               "paint" = paint_color,
               "steel" = steel_color, 
               "concrete" = concrete_color)

forecast_colors <- c("BCI" = bci_color, 
                     "Forecast" = forecast_color)

#### ENR Indices & DF ####

# ENR Data

enr_indices <- read.csv("enr_indices.csv")
enr_indices$Date <- as.Date(enr_indices$Date)

enr_indices <- enr_indices[
  enr_indices$Date >= "1993-01-01",]

enr_indices <- enr_indices %>%
  filter(row_number() <= n()-2)

# ENR Normalized (min-max)

enr_scaled <- as.data.frame(lapply(enr_indices[2:6], min_max))
enr_scaled$Date <- enr_indices$Date

#### FRED Indices & DF ####

# Wages

# federal min wage 

fmw <- "FEDMINFRMWG"

fed_min <- api_data_pro(fed_min, fmw, col1 = "Date", 
                        col2 = "fed_min_wage")

#california min wage 

cmw <- "STTMINWGCA"

ca_min <- api_data_pro(ca_min, cmw, col1 = "Date", 
                       col2 = "cal_min_wage")

# construction wages (non-super)

cwns <- "CES2000000008"

const_ns_wage <- api_data_pro(const_ns_wage, cwns, 
                              col1 = "Date", 
                              col2 = "const_ns_wage")

# Material

# lumber

lwp <- "WPU08"

lumber <- api_data_pro(lumber, lwp, col1 = "Date", 
                       col2 = "lumber")

# concrete

cnc <- "PCU327320327320"

concrete <- api_data_pro(concrete, cnc, col1 = "Date", 
                         col2 = "concrete")

# Steel

smp <- "WPU1017"

steel <- api_data_pro(steel, smp, col1 = "Date", 
                      col2 = "steel")

# Paint

pnt <- "PCU325510325510"

paint <- api_data_pro(paint, pnt, col1 = "Date", 
                      col2 = "paint")

# Ttl Construction Spending

ttl <- "TTLCONS"

ttl_cnst <- api_data_pro(ttl_cnst, ttl, col1 = "Date", 
                      col2 = "TTL Construction Spending")

# wage tables

# FRED wage data

fred_tables <- list(fed_min, ca_min, const_ns_wage)
fred_data <- fred_tables %>%
  reduce(full_join, by="Date")

fred_data <- fred_data %>% 
  fill(cal_min_wage)

fred_scaled <- as.data.frame(lapply(fred_data[2:4], min_max))
fred_scaled$Date <- fred_data$Date 

#### Merged Data ####

# labor data

lab_enr <- enr_indices

labor <- merge(lab_enr, fred_data, by = "Date")

labor <- labor %>%
  select(-one_of("BCI", "CCI", "MPI"))

lab_scaled <- as.data.frame(lapply(labor[2:6], min_max))
lab_scaled$Date <- labor$Date

# Material

mat_enr <- enr_indices

mat_tables <- list(mat_enr, lumber, paint, 
                   steel, concrete)

mat <- mat_tables %>% 
  reduce(full_join, by = "Date")

mat <- mat %>%
  select(-one_of("BCI", "CCI", "CLI", "SLI"))

mat_scaled <- as.data.frame(lapply(mat[2:6], min_max))
mat_scaled$Date <- mat$Date

# All data

all_data_list <- list(enr_indices, ttl_cnst, fred_data, lumber, paint, 
                      steel, concrete)

all_data <- all_data_list %>%
  reduce(full_join, by = "Date")


#### Archive/Test Snippets ####
# Data shape (not in use)

# log transformation test

ggplot(fred_data, aes(x = const_ns_wage)) + 
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(alpha = .2) + 
  geom_vline(aes(xintercept = mean(const_ns_wage)))

fred_log <- as.data.frame(lapply(fred_data[2:4], log_scale))
fred_log$Date <- fred_data$Date

ggplot(fred_log, aes(x = Date, y = const_ns_wage)) +
  geom_line(color = const_ns_color) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Construction Wages", y = "", x = "")

ggplot(fred_log, aes(Date)) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage")) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage")) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage")) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Wage Data", y = "", x = "")

ggplot(fred_log, aes(x = const_ns_wage)) + 
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(alpha = .2) + 
  geom_vline(aes(xintercept = mean(const_ns_wage)))

# square root transformation test

fred_sq <- as.data.frame(lapply(fred_data[2:4], sqr_scale))
fred_sq$Date <- fred_data$Date

ggplot(fred_sq, aes(x = Date, y = const_ns_wage)) +
  geom_line(color = const_ns_color) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Construction Wages", y = "", x = "")

ggplot(fred_sq, aes(Date)) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage")) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage")) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage")) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Wage Data", y = "", x = "")

ggplot(fred_sq, aes(x = const_ns_wage)) + 
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(alpha = .2) + 
  geom_vline(aes(xintercept = mean(const_ns_wage)))

# cube root transformation test


fred_cb <- as.data.frame(lapply(fred_data[2:4], cubr_scale))
fred_cb$Date <- fred_data$Date

ggplot(fred_cb, aes(x = Date, y = const_ns_wage)) +
  geom_line(color = const_ns_color) +
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Construction Wages", y = "", x = "")

ggplot(fred_cb, aes(Date)) +
  geom_line(aes(y = fed_min_wage, color = "fed_min_wage")) +
  geom_line(aes(y = cal_min_wage, color = "cal_min_wage")) +
  geom_line(aes(y = const_ns_wage, color = "const_ns_wage")) +
  scale_color_manual(values = wage_colors, name = "Indices") + 
  theme(plot.background = element_rect(fill = plot_color), 
        panel.background = element_rect(fill = plot_color), 
        panel.grid.major = element_line(colour = plot_line_color, 
                                        linetype = plot_line_type, 
                                        linewidth = plot_line)) +
  labs(title = "Wage Data", y = "", x = "")

ggplot(fred_cb, aes(x = const_ns_wage)) + 
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(alpha = .2) + 
  geom_vline(aes(xintercept = mean(const_ns_wage)))