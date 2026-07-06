#### Packages & Global####

library(dplyr)
library(tidyverse)
library(ggfortify)
library(reshape2)
library(fredr)
library(patchwork)
library(zoo)

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
  guides(color = guide_legend(title = "Index"))

ggplot(bci_cci2, aes(date)) +
  geom_line(aes(y = bci, color = "BCI")) +
  geom_line(aes(y = cci, color = "CCI")) +
  geom_line(aes(y = avg, color = "AVG")) +
  guides(color = guide_legend(title = "Index"))

bci_diff <- diff(bci_cci2$bci)
cci_diff <- diff(bci_cci2$cci)
