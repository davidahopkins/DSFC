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

#### PAR data processing ####

par <- read_csv("PAR_data.csv")

par$start_date <- as.Date(par$start_date, format = "%m/%d/%Y")
par$end_date <- as.Date(par$end_date, format = "%m/%d/%Y")

par$duration <- difftime(par$end_date, par$start_date)
par$duration <- as.numeric(gsub("\\D", "", par$duration))

par <- select(par, -c(5,6))
par$PAR <- par$total_cost / par$duration

par <- select(par, -c(6))

ggplot(par, aes(x = project_type, y = PAR)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "", y = "")

ggplot(par, aes(fill=ownership, x = project_type, y = PAR)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "", y = "") + 
  scale_fill_manual("ownership", values = c("coral2", "gray", "steelblue"))

ggplot(par, aes(fill=location, x = project_type, y = PAR)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "", y = "") + 
  scale_fill_manual("location", values = c("coral2", "gray", "steelblue", "skyblue"))
