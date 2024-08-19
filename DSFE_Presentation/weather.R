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
library(readr)

#### weather data processing ####

precip <- read_csv("noaa_data.csv")
head(precip)

ggplot(precip, aes(x = DATE, y = PRCP)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Rainfall")

precip$day <- format(precip$DATE, format = "%b %d")
precip$month <- format(as.Date(precip$DATE, format = "%y-%m-%d"), "%b")
precip$year <- year(ymd(precip$DATE))
head(precip)

precip_m <- precip %>% 
  group_by(day) %>%
  summarize(mean_rain = mean(PRCP, na.rm = TRUE)) %>%
  filter(mean_rain > 0.125)

ggplot(precip_m, aes(x = day, y = mean_rain)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Rainfall")

precip_avg <- precip %>% 
  filter(PRCP > 0.125) %>%
  group_by(month) %>%
  summarize(rain_days = n() / 6)

ggplot(precip_avg, aes(x = month, y = rain_days)) +
  geom_bar(position="dodge", stat="identity", fill="coral2") +
  labs(x = "Date", y = "Days Rainfall > 0.125")

avgt <- read_csv("avgt.csv")
avgt <- avgt[-c(1:3),]
names(avgt) <- c("date", "avg_temp")
avgt$avg_temp <- as.numeric(avgt$avg_temp)
avgt$date <- ym(avgt$date)
head(avgt)

mint <- read_csv("mint.csv")
mint <- mint[-c(1:3),]
names(mint) <- c("date", "min_temp")
mint$min_temp <- as.numeric(mint$min_temp)
mint$date <- ym(mint$date)
head(mint)  

maxt <- read_csv("maxt.csv")
maxt <- maxt[-c(1:3),]
names(maxt) <- c("date", "max_temp")
maxt$max_temp <- as.numeric(maxt$max_temp)
maxt$date <- ym(maxt$date)
head(maxt)  

temp <- merge(mint, avgt, by="date")
temp <- merge(temp, maxt, by="date")

min_max_tmp <- temp

temp$max_temp <- temp$max_temp - temp$avg_temp
temp$avg_temp <- temp$avg_temp - temp$min_temp

temp$date <- format(as.Date(temp$date, 
                             format = "%y-%m-%d"), "%m")

head(temp)

temp <- temp %>% 
  group_by(date) %>%
  summarize(mean_min_temp = mean(min_temp, na.rm = TRUE),
            mean_avg_temp = mean(avg_temp, na.rm = TRUE),
            mean_max_temp = mean(max_temp, na.rm = TRUE)
            )

names(temp) <- c("month", "minimum", "average",
                 "maximum")

head(temp)

temp<- temp %>% 
  pivot_longer(cols = c("minimum", "average", 
                        "maximum"), 
               names_to = "minimum", 
               values_to = "avgerage")

head(temp)

names(temp) <- c("month", "temp_range", "temp")

temp<- temp %>%
  mutate(month = month.abb[as.numeric(month)])

head(temp)

  
ggplot(temp, aes(fill=temp_range, y=value, x= month)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr",
                            "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec")) +
  labs(y = "", x = "") +
  scale_fill_manual("temp_range", 
                    values = c("coral2", "steelblue", "skyblue"))

#### weather regression ####

tmp_pro <- read_csv("TMPvPRO.csv")

head(tmp_pro)

ggplot(tmp_pro, aes(x = Temp, y = Production)) +
  geom_point(color = "coral2")

tmp_reg <- lm(Production ~ poly(Temp, 2, raw = T), data = tmp_pro)
summary(tmp_reg)

ggplot(tmp_pro, aes(x = Temp, y = Production)) +
  geom_point(color = "coral2") + 
  stat_smooth(method="lm", formula = y ~ poly(x,2))

temp <- 36
y_i <- 116.529
c2 <- 1.906658
c3 <- -0.01361731

production <- y_i + (c2*temp) + (c3*(temp^2))
production

min_max_tmp$date <- format(as.Date(min_max_tmp$date, 
                            format = "%y-%m-%d"), "%m")

min_max_tmp <- min_max_tmp %>% 
  group_by(date) %>%
  summarize(mean_min_temp = mean(min_temp, na.rm = TRUE),
            mean_avg_temp = mean(avg_temp, na.rm = TRUE),
            mean_max_temp = mean(max_temp, na.rm = TRUE)
  )

names(min_max_tmp) <- c("month", "minimum", "average",
                 "maximum")

min_max_tmp

est <- read_csv("div_est.csv")

#### weather adjustments ####
