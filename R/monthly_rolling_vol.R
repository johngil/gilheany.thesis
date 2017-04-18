data(final_rolling_vol)
library(base)

# Month end reported Rolling Vols
monthly_rolling_vol <- final_rolling_vol[endpoints(final_rolling_vol$Date, on = "months"), ]
monthly_rolling_vol$Date <- as.Date(monthly_rolling_vol$Date)
