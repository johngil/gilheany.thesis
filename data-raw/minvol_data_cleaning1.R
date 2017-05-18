View(minvol)
library(dplyr)
# Get rid of cash and the weird companies with numbers that have no ticker
# minvol <- minvol %>% filter(complete.cases(ticker))

#Create monthly data set with just the tickers and dates from the minvol data
month_data <- select(minvol, ticker, date)
month_data <- arrange(month_data, ticker, date)

# I see some numeric tickers that should be removed. A60, 6COP, etc. Will find them all:
# Subset to exchanges not NYSE and NASDAQ
nonminvol_ex <- filter(minvol, exchange != "New York Stock Exchange Inc.", exchange != "NASDAQ")

# I did a check for the data with the two exchanges above, and data is perfect

# Find unique tickers in this nonminvol_ex data set
library(tidyr)
unique(nonminvol_ex$ticker)

# First remove values from the minvol data set that had NA values for the tickers
# minvol <- minvol %>% drop_na(ticker)

# Saw more cash assets that need to be dropped as well
# minvol1 <- filter(minvol, sector == "Cash and/or Derivatives") # 59 total observations
minvol <- filter(minvol, sector != "Cash and/or Derivatives")
minvol <- filter(minvol, sector != "S-T Securities")

# Looked through all the stocks, and changed tickers that were incorrect. See methods
minvol$ticker[minvol$ticker == "AAPL*"] <- "AAPL"
minvol$ticker[minvol$ticker == "A60"] <- "ACT"
minvol$ticker[minvol$ticker == "AG4"] <- "AGN"
minvol$ticker[minvol$ticker == "GGQ7"] <- "GOOGL"
minvol$ticker[minvol$ticker == "INCO"] <- "INTC"
minvol$ticker[minvol$ticker == "LOM"] <- "LMT"
minvol$ticker[minvol$ticker == "KTF"] <- "MDLZ"
minvol$ticker[minvol$ticker == "QCI"] <- "QCOM"
minvol$ticker[minvol$ticker == "RTN1"] <- "RTN"
minvol$ticker[minvol$ticker == "PA9"] <- "TRV"

# Additional updates
minvol$ticker[minvol$ticker == "VISA"] <- "V"
minvol$ticker[minvol$ticker == "LDOS"] <- "SAI"
minvol$ticker[minvol$ticker == "ANTM"] <- "WLP"
minvol$ticker[minvol$ticker == "BRKB"] <- "BRK"

# 4 tickers have NA values
# Will add tickers for the two by creating new data set, then removing the old data and rbinding it
SAI <- filter(minvol, name == "SAIC INC")
SAI$ticker <- "SAI"
SAI$ticker <- as.factor(SAI$ticker)

ANTM <- filter(minvol, name == "ANTHEM INC")
ANTM$ticker <- "ANTM"
ANTM$ticker <- as.factor(ANTM$ticker)

WLP <- filter(minvol, name == "WELLPOINT INC")
WLP$ticker <- "WLP"
WLP$ticker <- as.factor(WLP$ticker)

BRK <- filter(minvol, name == "BERKSHIRE HATHAWAY INC CLASS B")
BRK$ticker <- "BRK"
BRK$ticker <- as.factor(BRK$ticker)

# Drop NA tickers
library(tidyquant)
minvol <- minvol %>% drop_na(ticker)

# Rbind X and ANR to minvol data set
minvol <- rbind(minvol, SAI)
minvol <- rbind(minvol, ANTM)
minvol <- rbind(minvol, WLP)
minvol <- rbind(minvol, BRK)

minvol$ticker <- as.factor(minvol$ticker)

# Arrange by ticker
minvol <- arrange(minvol, ticker)

# minvol data set has dates for 2017-01-05. Remove then
minvol <- filter(minvol, date != "2017-01-05")
minvol$date <- as.Date(minvol$date)

# Several companies change names or tickers midway through data set
# have to manually input them

# **MHFI, S&P GLOBAL INC, change to MHP
# until 2013-04-30, then leave it as MHFI
MHFI <- filter(minvol, name == "S&P GLOBAL INC")
MHFI$ticker <- "MHFI"
MHFI$ticker <- as.factor(MHFI$ticker)
MHFI$date <- as.Date(MHFI$date)

MHFI1 <- filter(MHFI, date <= "2013-04-30")
MHFI1$ticker <- "MHP"

MHFI <- filter(MHFI, date > "2013-04-30")

MHFI <- rbind(MHFI, MHFI1)

minvol <- minvol[!(minvol$name=="S&P GLOBAL INC"),]

minvol <- rbind(minvol, MHFI)


# ** GOOGL, GOOGLE INC CLASS A
# change to GOOG until 2014-03-31, then leave it
GOOGL <- filter(minvol, name == "GOOGLE INC CLASS A")
GOOGL$ticker <- "GOOGL"
GOOGL$ticker <- as.factor(GOOGL$ticker)
GOOGL$date <- as.Date(GOOGL$date)

GOOGL1 <- filter(GOOGL, date <= "2014-03-31")
GOOGL1$ticker <- "GOOG"

GOOGL <- filter(GOOGL, date > "2014-03-31")

GOOGL <- rbind(GOOGL, GOOGL1)

minvol <- minvol[!(minvol$name=="GOOGLE INC CLASS A"),]

minvol <- rbind(minvol, GOOGL)


# Now have updated minvol data set with all the tickers
# need to get the list of unique tickers, and get the WRDS data again
# Write the tickers into a .txt file to directly upload into WRDS
unique_tickers <- unique(minvol$ticker)

new_data <- write.table(unique_tickers, "/Users/johngilheany/downloads/unique_tickers.txt",
												sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

# We want to compare returns of minvol with weighted returns from minvol data set
monthly_returns_minvol <- select(minvol, date, ticker, weight)
monthly_returns_minvol$date <- as.Date(monthly_returns_minvol$date)



# Get price data from WRDS
# CRSP Daily Stock Data --> ticker and price data only for unique_tickers.txt
# X48325bf9cff9aff2 <- read_csv("~/Downloads/X48325bf9cff9aff2")
# minvol_prices <- X48325bf9cff9aff2
colnames(minvol_prices) <- c("permco", "date", "ticker", "price")
minvol_prices <- select(minvol_prices, date, ticker, price)
minvol_prices$price <- abs(minvol_prices$price)
library(lubridate)
minvol_prices$date <- ymd(minvol_prices$date)
minvol_prices$ticker <- as.factor(minvol_prices$ticker)

# Extract month end data, instead of daily
minvol_prices <- minvol_prices[endpoints(minvol_prices$date, on = "months"), ]


# unique tickers
unique_tickers <- unique(minvol$ticker)
monthly_returns_minvol1 <- matrix(ncol=4)
colnames(monthly_returns_minvol1) <- c("date", "ticker", "price", "delta")
library(quantmod)
library(dplyr)

# Calculate the delta, change in return (monthly)
for (n in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(minvol_prices, ticker == n)
	temp <- temp[complete.cases(temp),]

	# Compute deltas for each ticker on a monthly basis
	if (nrow(temp) > 1){
		ticker_data <- filter(minvol_prices, ticker == n)
		ticker_data <- select(ticker_data, date, ticker, price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$delta <- Delt(ticker_data$price)
		ticker_data <- select(ticker_data, date, ticker, price, delta)
	}
	else {
	}

	monthly_returns_minvol1 <- rbind(monthly_returns_minvol1, ticker_data)
	monthly_returns_minvol1$date <- as.Date(monthly_returns_minvol1$date)
}

# Get rid of NA values
monthly_returns_minvol1 <- monthly_returns_minvol1 %>% filter(complete.cases(delta))

# Merge with monthly_returns_minvol, by date and ticker
monthly_returns_minvol <- merge(monthly_returns_minvol, monthly_returns_minvol1, by = c("date", "ticker"), all.x = TRUE)

# Arrange by ticker
monthly_returns_minvol <- arrange(monthly_returns_minvol, ticker)

# Multiply returns by weight
monthly_returns_minvol$weighted_return <- monthly_returns_minvol$weight * monthly_returns_minvol$delta

# Issue with stock split data. DUK on 2012-07-31. Manually calculated it, and will change delta
monthly_returns_minvol <- within(monthly_returns_minvol, delta[ticker == 'DUK' & date == '2012-07-31'] <- 0.02023706)
monthly_returns_minvol <- within(monthly_returns_minvol, weighted_return[ticker == 'DUK' & date == '2012-07-31'] <- 0.04086672)

# Aggregate the returns based on date
minvol_returns_agg <- aggregate(weighted_return ~ date, data=monthly_returns_minvol, FUN=sum)

# Get minvol index return data for dates in returns1 data set
# get ticker and price from CRSP monthly return data
# X0409053f9a77e0d1 <- read_csv("~/Downloads/X0409053f9a77e0d1")
# eminvol_return <- X0409053f9a77e0d1
eminvol_return$date <- ymd(eminvol_return$date)
colnames(eminvol_return) <- c("id", "date", "ticker", "price")
# Some price values are negative for some reason --> convert to all positive
eminvol_return$price <- abs(eminvol_return$price)
eminvol_return$eminvol_return <- Delt(eminvol_return$price)
eminvol_return$eminvol_return <- eminvol_return$eminvol_return * 100
eminvol_return <- select(eminvol_return, date, eminvol_return)

returns2 <- merge(minvol_returns_agg, eminvol_return, by = "date")

# Remove 2011-10-31 for now, because NA value for index return (beginning of index)
returns2 <- returns2[!(returns2$date=="2011-10-31"),]
#returns2 <- returns2[!(returns2$date=="2011-11-30"),]

# Plot returns calculated individually vs returns from Eminvol index
df1<-data.frame(x=returns2$date,y=returns2$weighted_return)
df2<-data.frame(x=returns2$date,y=returns2$eminvol_return)

ggplot(returns2, aes(x=weighted_return, y=eminvol_return)) +
	geom_point(shape=1) +  geom_smooth(method=lm)

cor(returns2$eminvol_return, returns2$weighted_return)
