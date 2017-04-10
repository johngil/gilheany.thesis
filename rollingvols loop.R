library(dplyr)
library(quantmod)
data(usa)

# Unique tickers in USA data set, in alphabetical order
unique_tickers <- sort(unique(usa$Ticker))

final_rolling_vol <- matrix(ncol=3)
colnames(final_rolling_vol) <- c("Date", "Ticker", "Volatility")

for (ticker in unique_tickers){

	# Empty data set to add rolling vol data to
	# Set dimensions appropriately
	#rolling_vol <- matrix(ncol = 3, nrow = nrow(temp))
	#colnames(rolling_vol) <- c("Date", "Ticker", "Volatility")


	# Test file to make sure enough observations are available
	temp <- filter(usa, Ticker == ticker)
	temp <- select(temp, Date, Ticker, Price)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 12 non NA values
	if (nrow(temp) > 12){
		ticker_data <- filter(usa, Ticker == ticker)
		ticker_data <- select(ticker_data, Date, Ticker, Price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$Volatility <- runSD(ticker_data$Price, n=12)
		#ticker_data$Volatility <- sqrt(252) * runSD( ticker_data$Price, 12 )
		ticker_data <- select(ticker_data, Date, Ticker, Volatility)
		}
		# Put ticker data in to rolling_vol
	#rolling_vol <- ticker_data
	else {
	}

	final_rolling_vol <- rbind(final_rolling_vol, ticker_data)
	final_rolling_vol$Date <- as.Date(final_rolling_vol$Date)
}

library(ggvis)

unique_dates <- unique(final_rolling_vol$Date)
final_rolling_vol <- final_rolling_vol[complete.cases(final_rolling_vol),]
ggplot(final_rolling_vol, aes(Date, Volatility) + geom_line())


ggplot(final_rolling_vol, aes(Date, Volatility, group = Ticker)) +
	geom_point() +
	geom_line()

ggplot(final_rolling_vol, aes(Date, Volatility, group = Ticker)) + ylim(0, 100) + geom_line()

unique_tickers

# Write the tickers into a .txt file to directly upload into WRDS
write.table(unique_tickers, "/Users/johngilheany/downloads/unique_tickers.txt",
						sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE)




