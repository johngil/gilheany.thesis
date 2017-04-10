library(dplyr)
library(quantmod)

colnames(wrds_data) <- c("ID", "Date","Ticker","Price")

# Date column needs to be converted to date class
wrds_data$Date <- ymd(as.character(wrds_data$Date))

# Unique tickers in WRDS data set, in alphabetical order (already from .txt file)
unique_tickers <- sort(unique(wrds_data$Ticker))

final_rolling_vol <- matrix(ncol=3)
colnames(final_rolling_vol) <- c("Date", "Ticker", "Volatility")

for (ticker in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(wrds_data, Ticker == ticker)
	temp <- select(temp, Date, Ticker, Price)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 12 non NA values
	if (nrow(temp) > 252){
		ticker_data <- filter(wrds_data, Ticker == ticker)
		ticker_data <- select(ticker_data, Date, Ticker, Price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$Volatility <- runSD(ticker_data$Price, n=252)
		ticker_data <- select(ticker_data, Date, Ticker, Volatility)
	}
	else {
	}

	final_rolling_vol <- rbind(final_rolling_vol, ticker_data)
	final_rolling_vol$Date <- as.Date(final_rolling_vol$Date)
}

final_rolling_vol <- final_rolling_vol[complete.cases(final_rolling_vol),]

ggplot(final_rolling_vol, aes(Date, Volatility, group = Ticker)) +
	geom_line()

