#### Packages & Global####

library(dplyr)
library(tidyverse)
library(ggfortify)
library(reshape2)
library(fredr)
library(patchwork)
library(zoo)

#### Global Var ####

fredr_set_key("02068cf41e8c95b7ed918d614b9c459b")

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

PPI_RMC = "PCU327320327320"

#PPI ready-mix concrete materials (1989, mo)

PPI_RMI = "WPS133301"

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

api_data_pro <- function(df, id, df_start = p1_start, df_end = data_date,
                         multiplier = 1, col1 = "col1", col2 = "col2") {
  df <- fred_table(df, id, df_start, df_end)
  df <- select(df, -c(series_id, realtime_start, realtime_end))
  df$value <- df$value * multiplier
  colnames(df) <- c(col1, col2)
  return(df)
}

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

#### Employment ####

#All employees, construction (1939, mo, thousands of persons)

emp <- api_data_pro(emp, EMP_CON, "1939-01-01",
                    col1 = "date", col2 = "All Employees: Construction")
head(emp)

#job openings, construction (2001, mo, level in thousands)

job <- api_data_pro(job, JOB_OP, "2001-01-01",
                    col1 = "date", col2 = "Job Openings: Construction")
head(job)

#Not in labor force, want a job, search in previous year, available to work
#(1994, mo, thousands)

lbr <- api_data_pro(lbr, LBR_FRC, "1994-01-01",
                    col1 = "date", col2 = "Available Labor")
head(lbr)

#unemployment rate, construction, private wage and salary (2000, mo, percent)

unemp <- api_data_pro(unemp, UNEMP, "2000-01-01",
                    col1 = "date", col2 = "Unemployment Rate: Construction")
head(unemp)

emp2 <- period_select(emp, emp2, p1_start, p3_end)
job2 <- period_select(job, job2, p1_start, p3_end)
lbr2 <- period_select(lbr, lbr2, p1_start, p3_end)
unemp2 <- period_select(unemp, unemp2, p1_start, p3_end)

comb_emp <- emp2 %>% mutate(job2, lbr2, unemp2)
head(comb_emp)

ggplot(comb_emp, aes(date)) +
  geom_line(aes(y = `All Employees: Construction`, color = "All Employees: Construction")) +
  geom_line(aes(y = `Job Openings: Construction`, color = "Job Openings: Construction")) +
  geom_line(aes(y = `Available Labor`, color = "Available Labor")) + 
  guides(color = guide_legend(title = "")) + 
  labs(x = "Date", y = "Total in Thousands") +
  theme(legend.position = "bottom")

#### Earnings & Wages ####

#Average earnings, all employees, construction (2006, mo)

avg_earn <- api_data_pro(avg_earn, AVG_EARN, "2007-01-01",
                           col1 = "date", col2 = "AVG_EARN")
head(avg_earn)

#average earning, production and non-super, construction (1947, mo)

avg_earn_p <- api_data_pro(avg_earn_p, AVG_EARN_P, "2007-01-01",
                    col1 = "date", col2 = "AVG_EARN_P")
head(avg_earn_p)

comb_earn <- avg_earn %>% mutate(avg_earn_p)
head(comb_earn)

ggplot(comb_earn, aes(date)) +
  geom_line(aes(y = AVG_EARN, color = "Avg Earnings")) +
  geom_line(aes(y = AVG_EARN_P, color = "Avg Earnings: Non-Super")) +
  guides(color = guide_legend(title = "")) + 
  labs(x = "Date", y = "Earnings") +
  theme(legend.position = "bottom")

avg_earn_full <- api_data_pro(avg_earn_p, AVG_EARN_P, "1948-01-01",
                           col1 = "date", col2 = "AVG_EARN_FULL")
head(avg_earn_full)

ggplot(comb_earn, aes(date)) +
  geom_line(aes(y = AVG_EARN_P, color = "Avg Earnings: Non-Super")) +
  guides(color = guide_legend(title = "")) + 
  labs(x = "Date", y = "Earnings") +
  theme(legend.position = "bottom")

#### PPI Indices #####

#producer price index (1913, mo)

ppi <- api_data_pro(ppi, PPI, "2004-01-01",
                    col1 = "date", col2 = "PPI")
head(ppi)

#producer price index, by commodity, construction materials (1947, mo)

ppi_cm <- api_data_pro(ppi_cm, PPI_CM, "2004-01-01",
                    col1 = "date", col2 = "PPI_CM")
head(ppi_cm)

#PPI building material and supplies dealers (2004, mo, index)

ppi_bms <- api_data_pro(ppi_bms, PPI_BMS, "2004-01-01",
                       col1 = "date", col2 = "PPI_BMS")
head(ppi_bms)

#PPI by industry, Cement and Concrete Manufacturing (2004, mo, index)

ppi_ccm <- api_data_pro(ppi_ccm, PPI_CCM, "2004-01-01",
                       col1 = "date", col2 = "PPI_CCM")
head(ppi_ccm)


#PPI Metals and Metal Products (1926, mo, index)

ppi_mmp <- api_data_pro(ppi_mmp, PPI_MMP, "2004-01-01",
                       col1 = "date", col2 = "PPI_MMP")
head(ppi_mmp)

#PPI Paint and Coating Manufacturing (1984, mo, index)

ppi_pc <- api_data_pro(ppi_pc, PPI_PC, "2004-01-01",
                       col1 = "date", col2 = "PPI_PC")
head(ppi_pc)

#PPI Lumber and Wood Products (1926, mo, index)

ppi_lwp <- api_data_pro(ppi_lwp, PPI_LWP, "2004-01-01",
                       col1 = "date", col2 = "PPI_LWP")
head(ppi_lwp)

comb_ppi <- ppi %>% mutate(ppi_cm, ppi_bms, 
                           ppi_mmp, ppi_ccm, ppi_lwp, ppi_pc)
head(comb_ppi)

ggplot(comb_ppi, aes(date)) +
  geom_line(aes(y = PPI, color = "PPI")) +
  geom_line(aes(y = PPI_MMP, color = "PPI: Metal & Metal Products")) +
  geom_line(aes(y = PPI_CCM, 
                color = "PPI: Cement & Concrete Manufacturing")) +
  geom_line(aes(y = PPI_LWP, color = "PPI: Lumber & Wood Products")) + 
  geom_line(aes(y = PPI_PC, color = "PPI: Paints & Coatings")) +
  guides(color = guide_legend(title = "")) + 
  labs(x = "Date", y = "Index") +
  theme(legend.position = "right")

#### Construction Spending ####

#Total Construction spending: total construction in US (1993, mo, millions)



#Total Construction spending: total non-residential 
#construction in US (2002, mo, millions)


#Total Construction spending: total residential 
#construction in US (2002, mo, millions)


#Total Public Construction spending: 
#total construction in US (1993, mo, millions)



#Total Private Construction spending: 
#total construction in US (1993, mo, millions)


#Total Construction spending: total commercial 
#construction in US (2002, mo, millions)




#### GDP Indices ####

# Gross Domestic Product (1947, mo, billions)



# Real potential gross domestic product (1949, mo, billions)



#Federal Debt: Total Public Debt as Percent of GDP (1966, mo)



#### CPI Indices ####

#CPI for All Urban Consumers: All US Cities (1947, mo, index)


#CPI for All Urban Consumers: All US Cities less Food & Energy (1957, mo, index)


