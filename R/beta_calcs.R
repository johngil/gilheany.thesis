# Read in WRDS data for each ticker (Ticker, Price, S&P Return)
# wrds_data_beta <- c4af81a191300d4d
# colnames(wrds_data_beta) <- c("ID", "Date","Ticker","Price", "SPR")
library(lubridate)
data(wrds_data_beta)
library(base)

library(base)
# Convert classes
wrds_data_beta$Date <- ymd(as.character(wrds_data_beta$Date))
wrds_data_beta$Ticker <- as.factor(wrds_data_beta$Ticker)

library(quantmod)
library(dplyr)

# unique tickers
unique_tickers <- sort(unique(wrds_data_beta$Ticker))
beta_calcs <- matrix(ncol=3)
colnames(beta_calcs) <- c("Date", "Ticker", "Delta")

for (ticker in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(wrds_data_beta, Ticker == ticker)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 252 non NA values
	if (nrow(temp) > 252){
		ticker_data <- filter(wrds_data_beta, Ticker == ticker)
		ticker_data <- select(ticker_data, Date, Ticker, Price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$Delta <- Delt(ticker_data$Price)
		ticker_data <- select(ticker_data, Date, Ticker, Delta)
	}
	else {
	}

	beta_calcs <- rbind(beta_calcs, ticker_data)
	beta_calcs$Date <- as.Date(beta_calcs$Date)
}

# Arrange wrds_data_beta tickers in alphabetical order (same order as beta_calcs)
beta_calcs <- beta_calcs[complete.cases(beta_calcs),]

#Merge data sets by date and ticker so that Delta is now in wrds_data
wrds_data_beta <- merge(wrds_data_beta, beta_calcs, by = c("Ticker", "Date"))

library(PerformanceAnalytics)

# Need to convert classes
wrds_data_beta$Date <- ymd(as.character(wrds_data_beta$Date))
wrds_data_beta$Ticker <- as.factor(wrds_data_beta$Ticker)
