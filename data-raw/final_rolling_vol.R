# Rolling Vol loop calculation
library(dplyr)
library(quantmod)
library(base)

data(usa)

# Unique tickers in USA data set, in alphabetical order
unique_tickers <- sort(unique(usa$ticker))

# Data set with WRDS with 10 years of price data for each ticker
data1 <- a7c1a9ddc6ee5812
data1 <- select(data1, date, TICKER, PRC)
colnames(data1) <- c("date", "ticker", "price")
data1$date <- ymd(data1$date)

# Create data frame to store vol values
final_rolling_vol <- matrix(ncol=3)
colnames(final_rolling_vol) <- c("date", "ticker", "volatility")

for (tick in unique_tickers){

	# Empty data set to add rolling vol data to
	# Set dimensions appropriately
	rolling_vol <- matrix(ncol = 3, nrow = nrow(temp))
	colnames(rolling_vol) <- c("date", "ticker", "volatility")


	# Test file to make sure enough observations are available
	temp <- filter(data1, tick == ticker)
	temp <- select(temp, date, ticker, price)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 12 non NA values
	if (nrow(temp) > 12){
		ticker_data <- filter(data1, tick == ticker)
		ticker_data <- select(ticker_data, date, ticker, price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$volatility <- runSD(ticker_data$price, n=12)
		#ticker_data$volatility <- sqrt(252) * runSD( ticker_data$price, 12 )
		ticker_data <- select(ticker_data, date, ticker, volatility)
	}
	# Put ticker data in to rolling_vol
	#rolling_vol <- ticker_data
	else {
	}

	final_rolling_vol <- rbind(final_rolling_vol, ticker_data)
	final_rolling_vol$date <- as.Date(final_rolling_vol$date)
}

library(ggvis)

unique_dates <- unique(final_rolling_vol$date)
final_rolling_vol <- final_rolling_vol[complete.cases(final_rolling_vol),]
