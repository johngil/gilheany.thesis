# Monthly data for beta
library(base)

data(beta_values)
monthly_beta_values <- beta_values[endpoints(beta_values$Date, on = "months"), ]
monthly_beta_values <- select(monthly_beta_values, Date, Ticker, Beta)
#monthly_beta_values$Date <- as.Date(monthly_beta_values$Date)
