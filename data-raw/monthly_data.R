data(monthly_rolling_vol)
data(monthly_beta_values)

book_value <- select(book_value_data, Ticker, Date, PBR1)
book_value <- book_value[complete.cases(book_value),]

# monthly data for vol and beta and P/B
monthly_data1 <- merge(monthly_rolling_vol, monthly_beta_values, by = c("Ticker", "Date"), all=TRUE)
monthly_data2 <- merge(monthly_data1, book_value, by = c("Ticker", "Date"), all=TRUE)

# Create backbone of monthly data set --> subset data with
monthly1 <- select(usa, ticker, date)
monthly1$date <- as.Date(monthly1$date)
monthly_beta_values$ticker <- as.factor(monthly_beta_values$ticker)

# Fill in values for vol, beta --> keep monthly1 same, and fill in with beta values from other data set
colnames(monthly_beta_values) <- c("date", "ticker", "beta")
monthly2 <- merge(monthly1, monthly_beta_values, by = c("ticker", "date"), all.x = TRUE)
#colnames(final_rolling_vol) <- c("date","ticker","volatility")
monthly3 <- merge(monthly2, final_rolling_vol, by = c("ticker", "date"), all.x = TRUE)

#remove rows with na values in beta or vol
monthly3 <- monthly3 %>% drop_na(beta)
monthly3 <- monthly3 %>% drop_na(volatility)

# Fill in for P/B. Lag 3 months
#book_value_data <- select(book_value_data, date, tic, at, lt, bkvlps, csho, mkvalt, prcc_f)
#colnames(book_value_data) <- c("date", "ticker", "total_assets", "total_liabilities", "book_value_per_share", "shares_outstanding", "market_value", "stock_price")
book_value_data1 <- select(book_value_data, Date, Ticker, PBR1)
#remove rows with na values in P/B
book_value_data2 <- book_value_data1 %>% drop_na(PBR1)
#rename column names
colnames(book_value_data2) <- c("date", "ticker", "price_to_book")
book_value_data2$date <- ymd(book_value_data2$date)

# Lag each date in book value data set by 3 months
#book_value_data3 <- book_value_data2 %>% mutate(lag_date = date %m+% months(3))
book_value_data3$lag_date <- date %m+% months(3)
book_value_data4 <- select(book_value_data3, lag_date, ticker, price_to_book)
colnames(book_value_data4) <- c("date", "ticker","price_to_book")

# take backbone of dates and tickers from beta calcs
book_value_data5 <- select(beta_calcs, Date, Ticker)
colnames(book_value_data5) <- c("date", "ticker")
book_value_data6 <- merge(book_value_data5, book_value_data4, by = c("ticker", "date"), all.x = TRUE)
# fill in NA values for p/b with the most recent value on top
book_value_data6[1,3] = 0
book_value_data6$price_to_book <- na.locf(book_value_data6$price_to_book)

# merge p/b data with other monthly data
monthly4 <- merge(monthly3, book_value_data6, by = c("ticker", "date"), all.x = TRUE)
monthly5 <- monthly4[!duplicated(monthly4), ]



