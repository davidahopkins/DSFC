library(ggplot2)
library(tidyverse)

cash <- read_csv("Project Schedule Data.csv")
cash$`Project Number` <- as.factor(cash$`Project Number`)

ggplot(cash, aes(x = Date, y = `Cost Per Day`, color = `Project Number`)) + 
  geom_line()
