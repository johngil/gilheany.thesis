data(monthly_rolling_vol)
data(monthly_beta_values)

# monthly data for vol and beta
monthly_data <- merge(monthly_rolling_vol, monthly_beta_values, by = c("Ticker", "Date"))
