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

temp <- temp %>% 
  pivot_longer(cols = c("minimum", "average", 
                        "maximum"), 
               names_to = "minimum", 
               values_to = "avgerage")

head(temp)

names(temp) <- c("month", "temp_range", "temp")

temp <- temp %>%
  mutate(month = month.abb[as.numeric(month)])

head(temp)

  
ggplot(temp, aes(fill=factor(temp_range, 
                             levels=c("maximum", "average", "minimum")), 
                 y=temp, x= month)) +
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

### Var from Mean ###

tmp_pro$mean <- mean(tmp_pro$Production)

tmp_pro$diff <- tmp_pro$Production - tmp_pro$mean

ggplot(tmp_pro, aes(x = Temp, y = diff)) +
  geom_point(color = "coral2")

tmp_reg2 <- lm(diff ~ poly(Temp, 2, raw = T), data = tmp_pro)
summary(tmp_reg2)

ggplot(tmp_pro, aes(x = Temp, y = diff)) +
  geom_point(color = "coral2") + 
  stat_smooth(method="lm", formula = y ~ poly(x,2))

day_tmp <- 40
y_i <- -61.02194
c2 <- 1.906658
c3 <- -0.01361731

production <- y_i + (c2*day_tmp) + (c3*(day_tmp^2))
production

#### estimate processing ####

min_max_tmp$date <- format(as.Date(min_max_tmp$date, 
                                   format = "%y-%m-%d"), "%m")

min_max_tmp <- min_max_tmp %>% 
  group_by(date) %>%
  summarize(mean_min_temp = mean(min_temp, na.rm = TRUE),
            mean_avg_temp = mean(avg_temp, na.rm = TRUE),
            mean_max_temp = mean(max_temp, na.rm = TRUE)
  )

names(min_max_tmp) <- c("t_month", "minimum", "average",
                        "maximum")

min_max_tmp$t_month <- as.numeric(min_max_tmp$t_month)

min_max_tmp

est <- read_csv("div_est.csv")

est$WBS <- NULL
est$UOM <- NULL

est$Start <- mdy(est$Start)
est$End <- mdy(est$End)
est$last_mo_day <- ceiling_date(ymd(est$Start), "month") - days(1)

est$year <- year(ymd(est$Start))
est$month <- month(ymd(est$Start))
est$day <- day(ymd(est$Start))

est$f_m_days <- difftime(est$last_mo_day, est$Start)
est$f_m_days <- as.numeric(gsub("\\D", "", est$f_m_days))

est$e_m_days <- difftime(est$End, est$last_mo_day)
est$e_m_days <- as.numeric(gsub("\\D", "", est$e_m_days))

est$p_s_mo <- est$f_m_days / est$Duration
est$P_e_mo <- est$e_m_days / est$Duration

est$t_month <- if_else(est$p_s_mo>0.5, est$Start, est$End)
est$t_month <- format(as.Date(est$t_month, 
                                   format = "%y-%m-%d"), "%m")
est$t_month <- as.numeric(est$t_month)

est <- left_join(est, min_max_tmp, by="t_month")

est$temp <- if_else(est$maximum>80, est$maximum, est$minimum)

est$adj_crew_hrs <- est$Qty / 
  (est$Production + (y_i + (c2*est$temp) + (c3*(est$temp^2))))

est$adj_lbr_cost <- est$adj_crew_hrs * est$`Labor Rate`

est_res <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(est_res) <- c("start_mo", "ttl_crew_hrs", "ttl_lbr_cost", 
                       "adj_crew_hrs", "adj_lbr_cost")

results <- data.frame(start_mo = c(est$month[1]),
                      ttl_crew_hrs = sum(est$`Labor Hours`), 
                      ttl_lbr_cost = sum(est$`Labor Total`), 
                      adj_crew_hrs = sum(est$adj_crew_hrs),
                      adj_lbr_cost = sum(est$adj_lbr_cost)
                      )

est_res <- rbind(est_res, results)
#rbind(est_res, results)

#### weather adjustments ####

est$month <- if_else(est$month == 12, 1, est$month + 1)

est$Start <- as.Date(paste(est$year, est$month, est$day), "%Y%m%d")
est$End <- est$End + est$Duration

est <- est %>% select(-minimum, -average, -maximum)
