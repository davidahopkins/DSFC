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


#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")

#current data date (most recent update)

data_date = "2024-06-01"

SD_UN = "LAUCN060730000000004"
SD_EMP_CON = "SMU06417402000000001SA"
SD_EMP = "LAUCN060730000000005"
CON_UR = "LNU04032231"
CON_UN = "LNU03032231"
US_UN = "LNU03000000"

#### Functions ####

fred_table <- function(df, id, start_date, end_date) {
  df <- fredr(
    series_id = id,
    observation_start = as.Date(start_date),
    observation_end = as.Date(end_date)
  )
}

api_data_pro <- function(df, id, df_start = p1_start, df_end = data_date,
                         multiplier = 1, col1 = "col1", col2 = "col2") {
  df <- fred_table(df, id, df_start, df_end)
  df <- select(df, -c(series_id, realtime_start, realtime_end))
  df$value <- df$value * multiplier
  colnames(df) <- c(col1, col2)
  return(df)
}

#### data processing ####

sd_un <- api_data_pro(sd_un, SD_UN, df_start = "2004-01-01",
                                multiplier = 0.001, col1 = "date", 
                                col2 = "sd_unemp")

sd_emp_con <- api_data_pro(sd_emp_con, SD_EMP_CON, df_start = "2004-01-01",
                           col1 = "date", col2 = "con_emp")

sd_emp <- api_data_pro(sd_emp, SD_EMP, df_start = "2004-01-01",
                       multiplier = 0.001, col1 = "date", 
                       col2 = "employment")

con_ur <- api_data_pro(con_un, CON_UR, df_start = "2004-01-01",
                       multiplier = 0.01, col1 = "date", 
                       col2 = "con_ur")

con_un <- api_data_pro(con_un, CON_UN, df_start = "2004-01-01",
                       col1 = "date", 
                       col2 = "con_unemp")
us_un <- api_data_pro(con_un, US_UN, df_start = "2004-01-01",
                       col1 = "date", 
                       col2 = "us_unemp")

un_comp <- merge(con_un, us_un, by="date")

ggplot(un_comp, aes(date)) +
  geom_line(aes(y = con_unemp, 
                color = "Unemployment")) +
  geom_line(aes(y = us_unemp, 
                color = "Construction Unemployment")) +
  guides(color = guide_legend(title = "")) + 
  labs(x = "", y = "Total in Thoudsands") +
  theme(legend.position = "bottom")

sd_lab <- merge(sd_emp, sd_emp_con, by="date")
sd_lab <- merge(sd_lab, sd_un, by="date")
sd_lab <- merge(sd_lab, con_ur, by="date")

head(sd_lab)

sd_lab$sd_con_un <- sd_lab$sd_unemp * (sd_lab$con_emp / sd_lab$employment)
sd_lab$us_con_un <- sd_lab$sd_unemp * sd_lab$con_ur
sd_lab$lab_avail <- ((sd_lab$sd_con_un) + sd_lab$us_con_un) / 2

head(sd_lab)

ggplot(sd_lab, aes(date)) +
  geom_line(aes(y = sd_con_un, 
                color = "Construction Unemployment (SD Rate): San Diego")) +
  geom_line(aes(y = us_con_un, 
                color = "Construction Unemployment (US Rate): San Diego")) +
  geom_line(aes(y = lab_avail, 
                color = "Availabe Labor: San Diego")) +
  guides(color = guide_legend(title = "")) + 
  labs(x = "", y = "Total in Thoudsands") +
  theme(legend.position = "bottom")

sd_lab <- sd_lab[-c(2:7)]
head(sd_lab)

ggplot(sd_lab, aes(date)) +
  geom_line(aes(y = lab_avail, color = "Availabe Labor: San Diego")) + 
  guides(color = guide_legend(title = "")) + 
  labs(x = "", y = "Total in Thoudsands") +
  theme(legend.position = "bottom")

sd_lab %>% as_tsibble() -> sd_lab_ts

sd_lab_ts <- sd_lab_ts %>%
  mutate(date = yearmonth(date))

#### decomposition ####

dcmp <- sd_lab_ts %>%
  model(stl = STL(lab_avail))

components(dcmp)

components(dcmp) %>% autoplot() + 
  labs(x = "")

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(lab_avail, colour="gray") + 
  geom_line(aes(y=trend), colour="blue") +
  labs(y = "Total in Thousands", x = "",
       title = "")

#### moving averages ####

ma6 <- sd_lab_ts %>%
  mutate(`6-MA` = slider::slide_dbl(lab_avail, mean, 
                                    .before = 3, .after = 3,
                                    .complete = TRUE))

ma12 <- sd_lab_ts %>%
  mutate(`6-MA` = slider::slide_dbl(lab_avail, mean, 
                                    .before = 6, .after = 6,
                                    .complete = TRUE))

ma6 %>% autoplot(lab_avail, colour="gray") +
  geom_line(aes(y = `6-MA`), colour = "blue") + 
  labs(y = "Total in Thousands", x = "")

ma12 %>% autoplot(lab_avail, colour="gray") +
  geom_line(aes(y = `6-MA`), colour = "blue") + 
  labs(y = "Total in Thousands", x = "")

ma2x6 <- sd_lab_ts %>%
  mutate(
    `6-MA` = slider::slide_dbl(lab_avail, mean, 
                                    .before = 3, .after = 3,
                                    .complete = TRUE),
    `2x6-MA` = slider::slide_dbl(`6-MA`, mean,
                                 .before = 1,
                                 .after = 0,
                                 .complete = TRUE)
    )

ma2x6 %>% autoplot(lab_avail, colour="gray") +
  geom_line(aes(y = `2x6-MA`), colour = "blue") + 
  labs(y = "Total in Thousands", x = "")

ma2x12 <- sd_lab_ts %>%
  mutate(
    `12-MA` = slider::slide_dbl(lab_avail, mean, 
                               .before = 6, .after = 6,
                               .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                 .before = 1,
                                 .after = 0,
                                 .complete = TRUE)
  )

ma2x12 %>% autoplot(lab_avail, colour="gray") +
  geom_line(aes(y = `2x12-MA`), colour = "blue") + 
  labs(y = "Total in Thousands", x = "")

#### forecasting ####

# ETS

ets <- sd_lab_ts %>%
  model(ETS(lab_avail ~ error("A") + trend("N") + season("N")))

ets_fc <- ets %>%
  forecast(h = 12)

ets_fc %>% 
  autoplot(sd_lab_ts) + 
  geom_line(aes(y = .fitted), col="blue",
            data = augment(ets)) + 
  labs(y = "Total in Thousands", x = "")

ets2 <- sd_lab_ts %>%
  model(ETS(lab_avail))

report(ets2)

components(ets2) %>%
  autoplot()

ets2 %>%
  forecast(h = 12) %>%
  autoplot(sd_lab_ts) + 
  labs(y = "Total in Thousands", x = "")

# Holt-Winters' Multiplicative Method

hw <- sd_lab_ts %>%
  model(
    additive = ETS(lab_avail ~ error("A") + trend("A") + 
                     season("A")),
    multiplicative = ETS(lab_avail ~ error("M") + trend("A") + 
                           season("M"))
  )

#first letter is error type second is trend type 
#third is season type N = none, A = Additive, M = Multiplicative 
#Z = Auto ANN is simple exponetial smoothing MAM is holt-winter multi

hw_fc <- hw %>%
  forecast(h = 12)

hw_fc %>% 
  autoplot(sd_lab_ts, level = NULL) + 
  labs(y = "Total in Thousands", x = "") + 
  guides(colour = guide_legend(title = "Forecast"))

hw_fc_36 <- hw %>%
  forecast(h = 36)

hw_fc_36 %>% 
  autoplot(sd_lab_ts, level = NULL) + 
  labs(y = "Total in Thousands", x = "") + 
  guides(colour = guide_legend(title = "Forecast"))
