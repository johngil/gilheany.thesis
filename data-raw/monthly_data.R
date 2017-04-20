data(monthly_rolling_vol)
data(monthly_beta_values)

book_value <- select(book_value_data, Ticker, Date, PBR1)
book_value <- book_value[complete.cases(book_value),]

# monthly data for vol and beta and P/B
monthly_data1 <- merge(monthly_rolling_vol, monthly_beta_values, by = c("Ticker", "Date"), all=TRUE)
monthly_data2 <- merge(monthly_data1, book_value, by = c("Ticker", "Date"), all=TRUE)
