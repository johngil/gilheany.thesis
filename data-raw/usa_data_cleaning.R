
View(usa)
library(dplyr)
# Get rid of cash and the weird companies with numbers that have no ticker
usa1 <- usa %>% filter(complete.cases(ticker))

#Create monthly data set with just the tickers and dates from the usa data
month_data <- select(usa1, ticker, date)
month_data <- arrange(month_data, ticker, date)

# I see some numeric tickers that should be removed. A60, 6COP, etc. Will find them all:
# Subset to exchanges not NYSE and NASDAQ
nonusa_ex <- filter(usa, exchange != "New York Stock Exchange Inc.", exchange != "NASDAQ")

# I did a check for the data with the two exchanges above, and data is perfect

# Find unique tickers in this nonusa_ex data set
library(tidyr)
unique(nonusa_ex$ticker)

# First remove values from the usa data set that had NA values for the tickers
usa <- usa %>% drop_na(ticker)

# Saw more cash assets that need to be dropped as well
# usa1 <- filter(usa, sector == "Cash and/or Derivatives") # 59 total observations
usa <- filter(usa, sector != "Cash and/or Derivatives")

# Looked through all the stocks, and changed tickers that were incorrect. See methods
usa$ticker[usa$ticker == "AAPL*"] <- "AAPL"
usa$ticker[usa$ticker == "A60"] <- "ACT"
usa$ticker[usa$ticker == "8686"] <- "AFL"
usa$ticker[usa$ticker == "AG4"] <- "AGN"
usa$ticker[usa$ticker == "ANRZQ"] <- "ANR"
usa$ticker[usa$ticker == "AEC1"] <- "AXP"
usa$ticker[usa$ticker == "AIG*"] <- "AIG"
usa$ticker[usa$ticker == "ADM*"] <- "ADM"
usa$ticker[usa$ticker == "DG3"] <- "CE"
usa$ticker[usa$ticker == "SWG"] <- "SCHW"
usa$ticker[usa$ticker == "ENY"] <- "XEC"
usa$ticker[usa$ticker == "MX4A"] <- "CME"
usa$ticker[usa$ticker == "KIJ"] <- "CXO"
usa$ticker[usa$ticker == "6COP"] <- "COV"
usa$ticker[usa$ticker == "DSG"] <- "DKS"
usa$ticker[usa$ticker == "DC7"] <- "DFS"
usa$ticker[usa$ticker == "3EC"] <- "ETN"
usa$ticker[usa$ticker == "4XS"] <- "ESRX"
usa$ticker[usa$ticker == "FB*"] <- "FB"
usa$ticker[usa$ticker == "GEC"] <- "GE"
usa$ticker[usa$ticker == "GGQ7"] <- "GOOGL"
usa$ticker[usa$ticker == "2BH"] <- "HCA"
usa$ticker[usa$ticker == "ALD"] <- "HON"
usa$ticker[usa$ticker == "ILU"] <- "ILMN"
usa$ticker[usa$ticker == "INCO"] <- "INTC"
usa$ticker[usa$ticker == "LKI"] <- "LNKD"
usa$ticker[usa$ticker == "LKQ1"] <- "LKQ"
usa$ticker[usa$ticker == "LOM"] <- "LMT"
usa$ticker[usa$ticker == "LTR"] <- "L"
usa$ticker[usa$ticker == "MTZ"] <- "MTB"
usa$ticker[usa$ticker == "MWZ"] <- "MET"
usa$ticker[usa$ticker == "KTF"] <- "MDLZ"
usa$ticker[usa$ticker == "MOS*"] <- "MOS"
usa$ticker[usa$ticker == "NTH"] <- "NOC"
usa$ticker[usa$ticker == "PRU*"] <- "PRU"
usa$ticker[usa$ticker == "QCI"] <- "QCOM"
usa$ticker[usa$ticker == "RTN1"] <- "RTN"
usa$ticker[usa$ticker == "RN7"] <- "RF"
usa$ticker[usa$ticker == "SLBA"] <- "SLB"
usa$ticker[usa$ticker == "S*"] <- "S"
usa$ticker[usa$ticker == "SJR"] <- "SPN"
usa$ticker[usa$ticker == "T3W1"] <- "TWC"
usa$ticker[usa$ticker == "TWX*"] <- "TWX"
usa$ticker[usa$ticker == "TLK"] <- "TOL"
usa$ticker[usa$ticker == "TW6"] <- "TW"
usa$ticker[usa$ticker == "UUM"] <- "UNM"
usa$ticker[usa$ticker == "USX1"] <- "X"
usa$ticker[usa$ticker == "WFC*"] <- "WFC"

# From NYSE/NASDAQ info
usa$ticker[usa$ticker == "FCX*"] <- "FCX"
usa$ticker[usa$ticker == "UAC/C"] <- "UA"
usa$ticker[usa$ticker == "UAA"] <- "UA"
usa$ticker[usa$ticker == "BF.B"] <- "BF-B"
usa$ticker[usa$ticker == "BF/B"] <- "BF-B"

# KMI WS is a KINDER MORGAN EQUITY WARRANTS EXP --> removed
usa <- filter(usa, ticker != "KMI WS")

# ALPHA NATURAL RESOURCES AND US STEEL HAVE NA VALUES. WILL MANUALLY ADD TICKERS IN
# Will add tickers for the two by creating new data set, then removing the old data and rbinding it
X <- filter(usa, name == "US STEEL CORP CORP")
X$ticker <- "X"
ANR <- filter(usa, name == "ALPHA NATURAL RESOURCES INC")
ANR$ticker <- "ANR"
BF <- filter(usa, name == "BROWN FORMAN CORP CLASS B")
BF$ticker <- "BF-B"
BF$ticker <- as.factor(BF$ticker)

# Drop US Steel and Alpha resources by removing values with NA in ticker
usa <- usa %>% drop_na(ticker)

# Rbind X and ANR to usa data set
usa <- rbind(usa, X)
usa <- rbind(usa, ANR)
usa <- rbind(usa, BF)
usa$ticker <- as.factor(usa$ticker)

# Now have updated usa data set with all the tickers
# need to get the list of unique tickers, and get the WRDS data again
# Write the tickers into a .txt file to directly upload into WRDS
unique_tickers <- unique(usa$ticker)

new_data <- write.table(unique_tickers, "/Users/johngilheany/downloads/unique_tickers.txt",
												sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE)


# Create backbone of monthly data set --> subset data with
monthly1 <- select(usa, ticker, date)
monthly1$date <- as.Date(monthly1$date)
monthly_beta_values$ticker <- as.factor(monthly_beta_values$ticker)

# Fill in values for vol, beta --> keep monthly1 same, and fill in with beta values from other data set
colnames(monthly_beta_values) <- c("date", "ticker", "beta")
monthly2 <- merge(monthly1, monthly_beta_values, by = c("ticker", "date"), all.x = TRUE)
monthly3 <- merge(monthly2, final_rolling_vol, by = c("ticker", "date"), all.x = TRUE)

# Fill in for P/B. Lag 3 months
book_value_data <- select(book_value_data, datadate, tic, at, lt, bkvlps, csho, mkvalt, prcc_f)
colnames(book_value_data) <- c("date", "ticker", "total_assets", "total_liabilities", "book_value_per_share", "shares_outstanding", "market_value", "stock_price")
book_value_data$date <- ymd(book_value_data$date)
# Put in market cap values for dates given, and fill in the remaining values
market_cap <- select(book_value_data, date, ticker, market_value)
monthly4 <- merge(monthly3, market_cap, by = c("ticker", "date"), all.x = TRUE)
monthly4 <- monthly4 %>% fill(market_value)
# Put in book values for dates given, then lag them 3 months
# monthly1 has all the dates and tickers we are looking for
book_value <- select(book_value_data, date, ticker, book_value)
book_value1 <- merge(monthly1, book_value, by = c("ticker", "date"), all.x = TRUE)
# Do the following 3x for a 3 month lag
book_value1$book_value <- lag(book_value1$book_value)
# Can't really just blindly lag them.....need to do a loop where you are doing it individually
# But for now this is good enough -- will look at the rest later
book_value1 <- book_value1 %>% fill(book_value)
# merge book_value data with other monthly data
monthly5 <- merge(monthly4, book_value1, by = c("ticker", "date"), all.x = TRUE)
# Issue with filling in values of NAs that did not have --> could remove NA values first
# and then fill in later
monthly5$price_to_book <- monthly5$market_value/monthly5$book_value

-----------------------

# unique tickers
unique_tickers <- unique(usa$ticker)
returns <- matrix(ncol=5)
colnames(returns) <- c("weight", "date", "ticker", "price", "delta")

for (n in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(usa, ticker == n)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 252 non NA values
	if (nrow(temp) > 1){
		ticker_data <- filter(usa, ticker == n)
		ticker_data <- select(ticker_data, weight, date, ticker, price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$delta <- Delt(ticker_data$price)
		ticker_data <- select(ticker_data, weight, date, ticker, price, delta)
	}
	else {
	}

	returns <- rbind(returns, ticker_data)
	returns$date <- as.Date(returns$date)
}

# Multiply returns by weight
returns$weighted_return <- returns$weight * returns$delta
# Aggregate the returns based on date
returns1 <- aggregate(weighted_return ~ date, data=returns1, FUN=sum)

# Get EUSA index return data for dates in returns1 data set
eusa_return <- b340d740bd26c371
eusa_return$date <- ymd(eusa_return$date)
colnames(eusa_return) <- c("id", "date", "ticker", "price")
eusa_return$eusa_return <- Delt(eusa_return$price)
eusa_return$eusa_return <- eusa_return$eusa_return * 100
eusa_return <- select(eusa_return, date, price, eusa_return)

returns1 <- merge(returns1, eusa_return, by = "date")

# One price return is -32.6, which makes no sense. Became negative somehow
eusa_return[17, 2] = 32.6300

# Plot returns calculated individually vs returns from EUSA index
df1<-data.frame(x=returns1$date,y=returns1$weighted_return)
df2<-data.frame(x=returns1$date,y=returns1$eusa_return)

ggplot(df1,aes(x,y))+geom_line(aes(color="Calculated Return"))+
	geom_line(data=df2,aes(color="EUSA Index Return"))+
	labs(color="Legend text")

