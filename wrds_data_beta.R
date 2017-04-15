# Read in WRDS data for each ticker (Ticker, Price, S&P Return)
wrds_data_beta <- c4af81a191300d4d
colnames(wrds_data_beta) <- c("ID", "Date","Ticker","Price", "SPR")

# Convert classes
wrds_data_beta$Date <- ymd(as.character(wrds_data_beta$Date))
wrds_data_beta$Ticker <- as.factor(wrds_data_beta$Ticker)

library(quantmod)

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
# Can merge this data with the Vol data

# Can remove NA values if that is desireable
# wrds_beta_updated <- wrds_beta_updated[complete.cases(wrds_beta_updated),]

# Now have % change in stock, and % change in the S&P. Just need to find the 1-year betas
library(PerformanceAnalytics)





################################################################################################



# Need to convert classes
wrds_data_beta$Date <- ymd(as.character(wrds_data_beta$Date))
wrds_data_beta$Ticker <- as.factor(wrds_data_beta$Ticker)

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

class(beta_values$Date)


wrds_data_updated <- merge(wrds_data_updated, daily_rolling_sd_data, by = c("Ticker", "Date"))


# Monthly data for beta
monthly_beta_values <- beta_values[endpoints(beta_values$Date, on = "months"), ]
monthly_beta_values <- select(monthly_beta_values, Date, Ticker, Beta)
monthly_beta_values$Date <- as.Date(monthly_beta_values$Date)

# monthly data for vol and beta
monthly_data <- merge(monthly_rolling_vol, monthly_beta_values, by = c("Ticker", "Date"))

# Spaggeti plot for monthly trailing vol
ggplot(monthly_rolling_vol, aes(Date, Volatility, group = Ticker)) + geom_line()

# Spaggeti plot for monthly Beta
ggplot(monthly_beta_values, aes(Date, Beta, group = Ticker)) + geom_line()



