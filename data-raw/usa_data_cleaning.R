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
usa$ticker[usa$ticker == "MPN"] <- "MPC"
usa$ticker[usa$ticker == "H9B1"] <- "HTZ"
usa$ticker[usa$ticker == "NIHDQ"] <- "NIHD"
usa$ticker[usa$ticker == "PA9"] <- "TRV"
usa$ticker[usa$ticker == "UPLMQ"] <- "UPL"
usa$ticker[usa$ticker == "WLTGQ"] <- "WLT"

# From NYSE/NASDAQ info
usa$ticker[usa$ticker == "FCX*"] <- "FCX"
usa$ticker[usa$ticker == "UAC/C"] <- "UA"
usa$ticker[usa$ticker == "UAA"] <- "UA"
usa$ticker[usa$ticker == "BF/B"] <- "BF"

# Additional updates
usa$ticker[usa$ticker == "VISA"] <- "V"
usa$ticker[usa$ticker == "SPXC"] <- "SPW"
usa$ticker[usa$ticker == "GHC"] <- "WPO"
usa$ticker[usa$ticker == "LDOS"] <- "SAI"
usa$ticker[usa$ticker == "AAZ"] <- "APC"
usa$ticker[usa$ticker == "NCRA"] <- "NWS"
usa$ticker[usa$ticker == "ANTM"] <- "WLP"
usa$ticker[usa$ticker == "BRKB"] <- "BRK"

# KMI WS is a KINDER MORGAN EQUITY WARRANTS EXP --> removed
usa <- filter(usa, ticker != "KMI WS")

# Some tickers have NA values
# Will add tickers for the two by creating new data set, then removing the old data and rbinding it
X <- filter(usa, name == "US STEEL CORP CORP")
X$ticker <- "X"

ANR <- filter(usa, name == "ALPHA NATURAL RESOURCES INC")
ANR$ticker <- "ANR"

BF <- filter(usa, name == "BROWN FORMAN CORP CLASS B")
BF$ticker <- "BF"
BF$ticker <- as.factor(BF$ticker)

NIHD <- filter(usa, name == "NII HOLDINGS INC CLASS B")
NIHD$ticker <- "NIHD"
NIHD$ticker <- as.factor(NIHD$ticker)

UPL <- filter(usa, name == "ULTRA PETROLEUM CORP")
UPL$ticker <- "UPL"
UPL$ticker <- as.factor(UPL$ticker)

WLT <- filter(usa, name == "WALTER ENERGY INC")
WLT$ticker <- "WLT"
WLT$ticker <- as.factor(WLT$ticker)

WPO <- filter(usa, name == "GRAHAM HOLDINGS COMPANY CO CLASS B")
WPO$ticker <- "WPO"
WPO$ticker <- as.factor(WPO$ticker)

SAI <- filter(usa, name == "SAIC INC")
SAI$ticker <- "SAI"
SAI$ticker <- as.factor(SAI$ticker)

ANTM <- filter(usa, name == "ANTHEM INC")
ANTM$ticker <- "ANTM"
ANTM$ticker <- as.factor(ANTM$ticker)

WLP <- filter(usa, name == "WELLPOINT INC")
WLP$ticker <- "WLP"
WLP$ticker <- as.factor(WLP$ticker)

BRK <- filter(usa, name == "BERKSHIRE HATHAWAY INC CLASS B")
BRK$ticker <- "BRK"
BRK$ticker <- as.factor(BRK$ticker)

# Drop NA tickers
library(tidyquant)
usa <- usa %>% drop_na(ticker)

# Rbind X and ANR to usa data set
usa <- rbind(usa, X)
usa <- rbind(usa, ANR)
usa <- rbind(usa, BF)
usa <- rbind(usa, NIHD)
usa <- rbind(usa, UPL)
usa <- rbind(usa, WLT)
usa <- rbind(usa, SAI)
usa <- rbind(usa, WPO)
usa <- rbind(usa, ANTM)
usa <- rbind(usa, WLP)
usa <- rbind(usa, BRK)

usa$ticker <- as.factor(usa$ticker)

# Arrange by ticker
usa <- arrange(usa, ticker)

# Remove ORCHARD SUPPLY HARDWARE STORES tickers (OSHSQ and OSHWQ)
usa <- usa[-c(26965, 26966), ]

# usa data set has dates for 2017-01-05. Remove then
usa <- filter(usa, date != "2017-01-05")
usa$date <- as.Date(usa$date)

# Several companies change names or tickers midway through data set
# have to manually input them

# **LPT, LIBERTY PROPERTY REIT TRUST, change to LRY for dates up
# until 2014-01-31 (but leave as LPT for dates after that)
LPT <- filter(usa, name == "LIBERTY PROPERTY REIT TRUST")
LPT$ticker <- "LPT"
LPT$ticker <- as.factor(LPT$ticker)
LPT$date <- as.Date(LPT$date)

LPT1 <- filter(LPT, date <= "2014-01-31")
LPT1$ticker <- "LRY"

LPT <- filter(LPT, date > "2014-01-31")

LPT <- rbind(LPT, LPT1)

usa <- usa[!(usa$name=="LIBERTY PROPERTY REIT TRUST"),]

usa <- rbind(usa, LPT)


#**ZBH, ZIMMER BIOMET HOLDINGS INC, change to ZMH for dates up
# until 2014-11-28, but leave as is for dates after
ZBH <- filter(usa, name == "ZIMMER BIOMET HOLDINGS INC")
ZBH$ticker <- "ZBH"
ZBH$ticker <- as.factor(ZBH$ticker)
ZBH$date <- as.Date(ZBH$date)

ZBH1 <- filter(ZBH, date <= "2014-11-28")
ZBH1$ticker <- "ZMH"

ZBH <- filter(ZBH, date > "2014-11-28")

ZBH <- rbind(ZBH, ZBH1)

usa <- usa[!(usa$name=="ZIMMER BIOMET HOLDINGS INC"),]

usa <- rbind(usa, ZBH)


# **MHFI, S&P GLOBAL INC, change to MHP
# until 2013-04-30, then leave it as MHFI
MHFI <- filter(usa, name == "S&P GLOBAL INC")
MHFI$ticker <- "MHFI"
MHFI$ticker <- as.factor(MHFI$ticker)
MHFI$date <- as.Date(MHFI$date)

MHFI1 <- filter(MHFI, date <= "2013-04-30")
MHFI1$ticker <- "MHP"

MHFI <- filter(MHFI, date > "2013-04-30")

MHFI <- rbind(MHFI, MHFI1)

usa <- usa[!(usa$name=="S&P GLOBAL INC"),]

usa <- rbind(usa, MHFI)

#**NYCB, NEW YORK COMMUNITY BANCORP INC
#change to NYB until 2012-10-31, then leave the same
NYCB <- filter(usa, name == "NEW YORK COMMUNITY BANCORP INC")
NYCB$ticker <- "NYCB"
NYCB$ticker <- as.factor(NYCB$ticker)
NYCB$date <- as.Date(NYCB$date)

NYCB1 <- filter(NYCB, date <= "2012-10-31")
NYCB1$ticker <- "NYB"

NYCB <- filter(NYCB, date > "2012-10-31")

NYCB <- rbind(NYCB, NYCB1)

usa <- usa[!(usa$name=="NEW YORK COMMUNITY BANCORP INC"),]

usa <- rbind(usa, NYCB)


#**EA, ELECTRONIC ARTS INC
# change to ERTS until 2011-11-30. Then leave as EA.
EA <- filter(usa, name == "ELECTRONIC ARTS INC")
EA$ticker <- "EA"
EA$ticker <- as.factor(EA$ticker)
EA$date <- as.Date(EA$date)

EA1 <- filter(EA, date <= "2011-11-30")
EA1$ticker <- "ERTS"

EA <- filter(EA, date > "2011-11-30")

EA <- rbind(EA, EA1)

usa <- usa[!(usa$name=="ELECTRONIC ARTS INC"),]

usa <- rbind(usa, EA)

# **QVCA, LIBERTY INTERACTIVE QVC GROUP CORP
# LIBERTY INTERACTIVE CORP SERIES A (do this for both)
# change to LINTA until 2014-09-30 then leave as it
QVCA <- filter(usa, name == "LIBERTY INTERACTIVE QVC GROUP CORP" | name == "LIBERTY INTERACTIVE CORP SERIES A")
QVCA$ticker <- "QVCA"
QVCA$ticker <- as.factor(QVCA$ticker)
QVCA$date <- as.Date(QVCA$date)

QVCA1 <- filter(QVCA, date <= "2014-09-30")
QVCA1$ticker <- "LINTA"

QVCA <- filter(QVCA, date > "2014-09-30")

QVCA <- rbind(QVCA, QVCA1)

usa <- usa[!(usa$name == "LIBERTY INTERACTIVE QVC GROUP CORP" | usa$name == "LIBERTY INTERACTIVE CORP SERIES A"),]

usa <- rbind(usa, QVCA)

# **LB, L BRANDS INC
# change to LTD until 2013-11-29 then leave as is
LB <- filter(usa, name == "L BRANDS INC")
LB$ticker <- "LB"
LB$ticker <- as.factor(LB$ticker)
LB$date <- as.Date(LB$date)

LB1 <- filter(LB, date <= "2013-11-29")
LB1$ticker <- "LTD"

LB <- filter(LB, date > "2013-11-29")

LB <- rbind(LB, LB1)

usa <- usa[!(usa$name=="L BRANDS INC"),]

usa <- rbind(usa, LB)

# **ES, NORTHEAST UTILITIES
# change to NU until 2014-11-28, then leave be).
ES <- filter(usa, name == "NORTHEAST UTILITIES")
ES$ticker <- "NU"
ES$ticker <- as.factor(ES$ticker)
ES$date <- as.Date(ES$date)

ES1 <- filter(ES, date <= "2014-11-28")
ES1$ticker <- "NU"

ES <- filter(ES, date > "2014-11-28")

ES <- rbind(ES, ES1)

usa <- usa[!(usa$name=="NORTHEAST UTILITIES"),]

usa <- rbind(usa, ES)

# **JOY, JOY GLOBAL INC
# change to JOYG until 2011-11-30 then leave be
JOY <- filter(usa, name == "JOY GLOBAL INC")
JOY$ticker <- "JOY"
JOY$ticker <- as.factor(JOY$ticker)
JOY$date <- as.Date(JOY$date)

JOY1 <- filter(JOY, date <= "2011-11-30")
JOY1$ticker <- "JOYG"

JOY <- filter(JOY, date > "2011-11-30")

JOY <- rbind(JOY, JOY1)

usa <- usa[!(usa$name=="JOY GLOBAL INC"),]

usa <- rbind(usa, JOY)

#**VIAB, VIACOM INC CLASS B
# change to VIA up until 2011-11-30, then leave as is
VIAB <- filter(usa, name == "VIACOM INC CLASS B")
VIAB$ticker <- "VIAB"
VIAB$ticker <- as.factor(VIAB$ticker)
VIAB$date <- as.Date(VIAB$date)

VIAB1 <- filter(VIAB, date <= "2011-11-30")
VIAB1$ticker <- "VIA"

VIAB <- filter(VIAB, date > "2011-11-30")

VIAB <- rbind(VIAB, VIAB1)

usa <- usa[!(usa$name=="VIACOM INC CLASS B"),]

usa <- rbind(usa, VIAB)

# ** GOOGL, GOOGLE INC CLASS A
# change to GOOG until 2014-03-31, then leave it
GOOGL <- filter(usa, name == "GOOGLE INC CLASS A")
GOOGL$ticker <- "GOOGL"
GOOGL$ticker <- as.factor(GOOGL$ticker)
GOOGL$date <- as.Date(GOOGL$date)

GOOGL1 <- filter(GOOGL, date <= "2014-03-31")
GOOGL1$ticker <- "GOOG"

GOOGL <- filter(GOOGL, date > "2014-03-31")

GOOGL <- rbind(GOOGL, GOOGL1)

usa <- usa[!(usa$name=="GOOGLE INC CLASS A"),]

usa <- rbind(usa, GOOGL)


# Now have updated usa data set with all the tickers
# need to get the list of unique tickers, and get the WRDS data again
# Write the tickers into a .txt file to directly upload into WRDS
unique_tickers <- unique(usa$ticker)

new_data <- write.table(unique_tickers, "/Users/johngilheany/downloads/unique_tickers.txt",
												sep="\t", col.names = FALSE, row.names = FALSE, quote = FALSE)

# We want to compare returns of EUSA with weighted returns from usa data set
monthly_returns_usa <- select(usa, date, ticker, weight)
monthly_returns_usa$date <- as.Date(monthly_returns_usa$date)

# Get price data from WRDS
# CRSP Daily Stock Data --> ticker and price data only for unique_tickers.txt
# c40a80549103a679 <- read_csv("~/Downloads/fa8f8f8f04fe6522.csv")
# usa_prices <- c40a80549103a679
colnames(usa_prices) <- c("permco", "date", "ticker", "price")
usa_prices <- select(usa_prices, date, ticker, price)
# Some prices are negative for some reason....make positive
usa_prices$price <- abs(usa_prices$price)
library(lubridate)
usa_prices$date <- ymd(usa_prices$date)
usa_prices$ticker <- as.factor(usa_prices$ticker)

# Extract month end data, instead of daily
usa_prices <- usa_prices[endpoints(usa_prices$date, on = "months"), ]

# unique tickers
unique_tickers <- unique(usa$ticker)
monthly_returns_usa1 <- matrix(ncol=4)
colnames(monthly_returns_usa1) <- c("date", "ticker", "price", "delta")
library(quantmod)
library(dplyr)

# Calculate the delta, change in return (monthly)
for (n in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(usa_prices, ticker == n)
	temp <- temp[complete.cases(temp),]

	# Calculate the delta, change in return (monthly)
	if (nrow(temp) > 1){
		ticker_data <- filter(usa_prices, ticker == n)
		ticker_data <- select(ticker_data, date, ticker, price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$delta <- Delt(ticker_data$price)
		ticker_data <- select(ticker_data, date, ticker, price, delta)
	}
	else {
	}

	monthly_returns_usa1 <- rbind(monthly_returns_usa1, ticker_data)
	monthly_returns_usa1$date <- as.Date(monthly_returns_usa1$date)
}
# Get rid of NA values
monthly_returns_usa1 <- monthly_returns_usa1 %>% filter(complete.cases(delta))

# Merge with monthly_returns_usa, by date and ticker
monthly_returns_usa <- merge(monthly_returns_usa, monthly_returns_usa1, by = c("date", "ticker"), all.x = TRUE)

# Arrange by ticker
monthly_returns_usa <- arrange(monthly_returns_usa, ticker)

# Some duplicate rows
monthly_returns_usa <- unique(monthly_returns_usa)

# Multiply returns by weight
monthly_returns_usa$weighted_return <- monthly_returns_usa$weight * monthly_returns_usa$delta

# Some stock splits and other events not acocunted for properly on WRDS. Fixed by hand
# AAPL on 2014-06-30
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'AAPL' & date == '2014-06-30'] <- -0.02766193)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'AAPL' & date == '2014-06-30'] <- -0.08691378)

# LVLT 2011-10-31 15:1 reverse stock split, month end before is 1.49 * 15 = 22.35 to 26.69 next month
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'LVLT' & date == '2011-10-31'] <- 0.1941834)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'LVLT' & date == '2011-10-31'] <- 0.01638908)

# KO 2012-08-31 2:1 stock split
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'KO' & date == '2012-08-31'] <- -0.0802139)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'KO' & date == '2012-08-31'] <- -0.09214171)

# V 2015-03-31 4:1 stock split from 271.31 on 2015-02-27 to 65.41 (261.64 considering split)
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'V' & date == '2015-03-31'] <- -0.03538107)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'V' & date == '2015-03-31'] <- -0.02368055)

# MA 2014-01-31 10:1 stock split. From 835.46 on 2013-12-31 to 75.68 (756.80 considering split)
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'MA' & date == '2014-01-31'] <- -0.09415172)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'MA' & date == '2014-01-31'] <- -0.04422306)

# NFLX 2015-07-31 7:1 stock split. From 656.9400 on 2015-05-29 to 114.3100 (800.17 with split)
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'NFLX' & date == '2015-07-31'] <- 0.218026)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'NFLX' & date == '2015-07-31'] <- 0.04753534)

# CME 2012-07-31 5:1 stock split. From 268.110 to 52.110 (260.55)
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'CME' & date == '2012-07-31'] <- -0.02819738)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'CME' & date == '2012-07-31'] <- -0.003575428)

# Issue with stock split data. DUK on 2012-07-31. Manually calculated it, and will change delta
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'DUK' & date == '2012-07-31'] <- 0.02023706)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'DUK' & date == '2012-07-31'] <- 0.007443191)

# EVHC 2016-12-30 --> merger with AMSG, took their stock price. Will remove delta and weighted avg values
monthly_returns_usa <- within(monthly_returns_usa, delta[ticker == 'EVHC' & date == '2016-12-30'] <- 0)
monthly_returns_usa <- within(monthly_returns_usa, weighted_return[ticker == 'EVHC' & date == '2016-12-30'] <- 0)

# Aggregate the returns based on date
usa_returns <- aggregate(weighted_return ~ date, data=monthly_returns_usa, FUN=sum)

# Get EUSA index return data for dates in returns1 data set
# get ticker and price from CRSP monthly return data
# b9278c333f9fa857 <- read_csv("~/Downloads/b9278c333f9fa857")
# eusa_return <- b9278c333f9fa857
eusa_return$date <- ymd(eusa_return$date)
colnames(eusa_return) <- c("id", "date", "ticker", "price")
# Some price values are negative for some reason --> convert to all positive
eusa_return$price <- abs(eusa_return$price)
eusa_return$eusa_return <- Delt(eusa_return$price)
eusa_return$eusa_return <- eusa_return$eusa_return * 100
eusa_return <- select(eusa_return, date, eusa_return)

returns1 <- merge(usa_returns, eusa_return, by = "date")

# Remove 2011-10-31 for now, because many NAs
returns1 <- returns1[!(returns1$date=="2011-10-31"),]

# Plot returns calculated individually vs returns from EUSA index
df1<-data.frame(x=returns1$date,y=returns1$weighted_return)
df2<-data.frame(x=returns1$date,y=returns1$eusa_return)

ggplot(returns1, aes(x=weighted_return, y=eusa_return)) +
	geom_point(shape=1) +  geom_smooth(method=lm)

cor(returns1$eusa_return, returns1$weighted_return)

##########################################


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
