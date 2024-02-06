est_data <- data.frame(
  divisions = c("01 - General Conditions", "02 - Existing Conditions", "03 - Concrete"),
  price = c(5000, 15000, 35000),
  stringsAsFactors = FALSE)
est_data

est_data$cost <- c(3500, 12500, 27500)
est_data

add_div <- c("07 - Thermal & Moisture", 7500, 5500)
est_data <- rbind(est_data, add_div)
est_data$price <- as.numeric(est_data$price)
est_data$cost <- as.numeric(est_data$cost)
est_data

print(str(est_data))
print(summary(est_data))

est_data$margin <- ((est_data$price - est_data$cost) / est_data$price) * 100
est_data

for(i in c(-5, 10)) {
  est_data[, paste0("cost_", i, "%_change")] = 
    est_data$cost * (1+i/100)
  est_data[, paste0(i,"%_margin")] = 
    ((est_data$price - (est_data$cost * (1+i/100))) / est_data$price) * 100
}
est_data

ttl_cost <-sum(est_data$cost)
ttl_5_cost <- sum(est_data$`cost_-5%_change`)
ttl_10_cost <- sum(est_data$`cost_10%_change`)

cat(ttl_5_cost, ttl_cost, ttl_10_cost)