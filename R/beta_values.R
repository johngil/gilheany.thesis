data(wrds_data_beta)
library(base)


unique_tickers <- sort(unique(wrds_data_beta$Ticker))
unique_dates <- sort(unique(wrds_data_beta$Date))

beta_values <- matrix(ncol=5)
colnames(beta_values) <- c("Date", "Ticker", "Cov", "Var", "Beta")


for (ticker in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(wrds_data_beta, Ticker == ticker)
	temp <- select(temp, Date, Delta, SPR)
	temp <- temp[complete.cases(temp),]

	#	temp$Beta <- cov(temp$Delta, temp$SPR) / var(temp$SPR)
	if (nrow(temp) > 252){
		ticker_data <- filter(wrds_data_beta, Ticker == ticker)
		ticker_data <- select(ticker_data, Date, Ticker, Delta, SPR)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$Cov <- runCov(ticker_data$SPR, ticker_data$Delta, n=252)
		ticker_data$Var <- runVar(ticker_data$SPR, n = 252)
		ticker_data$Beta <- ticker_data$Cov/ticker_data$Var
		ticker_data <- select(ticker_data, Date, Ticker, Cov, Var, Beta)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$Date <- as.Date(ticker_data$Date)
	}
	else {
	}
	beta_values <- rbind(beta_values, ticker_data)
	beta_values$Date <- as.Date(beta_values$Date)
}

wrds_data_beta <- merge(wrds_data_beta, beta_values, by = c("Ticker", "Date"))
