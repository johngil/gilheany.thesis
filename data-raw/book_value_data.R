# BOOK VALUE DATA
library(lubridate)

data(book_value_data)

book_value_data$Date <- ymd(as.character(book_value_data$Date))

# Several variables are in millions. Need to convert multiple by 1000000
book_value_data$shares_outstanding <- book_value_data$shares_outstanding * 1000000
book_value_data$total_assets <- book_value_data$total_assets * 1000000
book_value_data$total_liabilities <- book_value_data$total_liabilities * 1000000
book_value_data$market_value <- book_value_data$market_value * 1000000

# Price per share can be calculated by divind market cap by shares outstanding
book_value_data$share_price <- book_value_data$market_value / book_value_data$shares_outstanding

# Find total Book value by multiplying book price per share times number of shares outstanding
book_value_data$Book_Value <- book_value_data$BV_per_share * book_value_data$Shares_Outstanding

# One way to find P/B is now to divide Price per share by Book Value per share
book_value_data$PBR1 <- book_value_data$Share_Price / book_value_data$BV_per_share

# Another way to find P/B is to divide Market Cap by Book Value
book_value_data$PBR2 <- book_value_data$Market_Value / book_value_data$Book_Value

# PBR1 and PBR2 should be equal, and they are.
