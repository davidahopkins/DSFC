#### Packages & Global####

library(dplyr)
library(tidyverse)
library(ggfortify)
library(reshape2)
library(fredr)
library(patchwork)
library(zoo)

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

month_matrix <- data.frame(mo_index = c(1:12), 
                           mo_abb = c("JAN", "FEB", "MAR", 
                                      "APR", "MAY", "JUN", 
                                      "JUL", "AUG", "SEP", 
                                      "OCT", "NOV", "DEC")) 

period_select <- function(df_1, df_2, start, end) {
  df_2 <- df_1
  df_2 <- df_2[df_2$date >= start & df_2$date <= end, ]
}

#current data date (most recent update)

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

#### ENR Indices ####

bci_table <- read.csv("bci.csv", stringsAsFactors = FALSE)
bci_table <- format_enr(bci_table)
colnames(bci_table) <- c('bci_index', 'bci_date')

cci_table <- read.csv("cci.csv", stringsAsFactors = FALSE)
cci_table <- format_enr(cci_table)
colnames(cci_table) <- c('cci_index', 'cci_date')

bci_cci <- bci_table
bci_cci <- select(bci_cci, 2, 1)
bci_cci$cci <- cci_table$cci_index
colnames(bci_cci) <- c('date', 'bci', 'cci')
bci_cci$avg <- (bci_cci$bci + bci_cci$cci) / 2

head(bci_cci)

bci_cci2 <- period_select(bci_cci, bci_cci2, p1_start, p3_end)

ggplot(bci_cci, aes(date)) +
  geom_line(aes(y = bci, color = "BCI")) +
  geom_line(aes(y = cci, color = "CCI")) +
  geom_line(aes(y = avg, color = "AVG")) +
  labs(y = "Index", x = "Time") +
  guides(color = guide_legend(title = "Index"))

ggplot(bci_cci2, aes(date)) +
  geom_line(aes(y = bci, color = "BCI")) +
  geom_line(aes(y = cci, color = "CCI")) +
  geom_line(aes(y = avg, color = "AVG")) +
  guides(color = guide_legend(title = "Index"))

bci_diff <- diff(bci_cci2$bci)
cci_diff <- diff(bci_cci2$cci)

