View(minvol)
library(dplyr)
# Get rid of cash and the weird companies with numbers that have no ticker
minvol1 <- minvol %>% filter(complete.cases(ticker))

#Create monthly data set with just the tickers and dates from the minvol data
month_data <- select(minvol1, ticker, date)
month_data <- arrange(month_data, ticker, date)

# I see some numeric tickers that should be removed. A60, 6COP, etc. Will find them all:
# Subset to exchanges not NYSE and NASDAQ
nonminvol_ex <- filter(minvol, exchange != "New York Stock Exchange Inc.", exchange != "NASDAQ")

# I did a check for the data with the two exchanges above, and data is perfect

# Find unique tickers in this nonminvol_ex data set
library(tidyr)
unique(nonminvol_ex$ticker)

# First remove values from the minvol data set that had NA values for the tickers
minvol <- minvol %>% drop_na(ticker)

# Saw more cash assets that need to be dropped as well
# minvol1 <- filter(minvol, sector == "Cash and/or Derivatives") # 59 total observations
minvol <- filter(minvol, sector != "Cash and/or Derivatives")

# Looked through all the stocks, and changed tickers that were incorrect. See methods
minvol$ticker[minvol$ticker == "AAPL*"] <- "AAPL"
minvol$ticker[minvol$ticker == "A60"] <- "ACT"
minvol$ticker[minvol$ticker == "8686"] <- "AFL"
minvol$ticker[minvol$ticker == "AG4"] <- "AGN"
minvol$ticker[minvol$ticker == "ANRZQ"] <- "ANR"
minvol$ticker[minvol$ticker == "AEC1"] <- "AXP"
minvol$ticker[minvol$ticker == "AIG*"] <- "AIG"
minvol$ticker[minvol$ticker == "ADM*"] <- "ADM"
minvol$ticker[minvol$ticker == "DG3"] <- "CE"
minvol$ticker[minvol$ticker == "SWG"] <- "SCHW"
minvol$ticker[minvol$ticker == "ENY"] <- "XEC"
minvol$ticker[minvol$ticker == "MX4A"] <- "CME"
minvol$ticker[minvol$ticker == "KIJ"] <- "CXO"
minvol$ticker[minvol$ticker == "6COP"] <- "COV"
minvol$ticker[minvol$ticker == "DSG"] <- "DKS"
minvol$ticker[minvol$ticker == "DC7"] <- "DFS"
minvol$ticker[minvol$ticker == "3EC"] <- "ETN"
minvol$ticker[minvol$ticker == "4XS"] <- "ESRX"
minvol$ticker[minvol$ticker == "FB*"] <- "FB"
minvol$ticker[minvol$ticker == "GEC"] <- "GE"
minvol$ticker[minvol$ticker == "GGQ7"] <- "GOOGL"
minvol$ticker[minvol$ticker == "2BH"] <- "HCA"
minvol$ticker[minvol$ticker == "ALD"] <- "HON"
minvol$ticker[minvol$ticker == "ILU"] <- "ILMN"
minvol$ticker[minvol$ticker == "INCO"] <- "INTC"
minvol$ticker[minvol$ticker == "LKI"] <- "LNKD"
minvol$ticker[minvol$ticker == "LKQ1"] <- "LKQ"
minvol$ticker[minvol$ticker == "LOM"] <- "LMT"
minvol$ticker[minvol$ticker == "LTR"] <- "L"
minvol$ticker[minvol$ticker == "MTZ"] <- "MTB"
minvol$ticker[minvol$ticker == "MWZ"] <- "MET"
minvol$ticker[minvol$ticker == "KTF"] <- "MDLZ"
minvol$ticker[minvol$ticker == "MOS*"] <- "MOS"
minvol$ticker[minvol$ticker == "NTH"] <- "NOC"
minvol$ticker[minvol$ticker == "PRU*"] <- "PRU"
minvol$ticker[minvol$ticker == "QCI"] <- "QCOM"
minvol$ticker[minvol$ticker == "RTN1"] <- "RTN"
minvol$ticker[minvol$ticker == "RN7"] <- "RF"
minvol$ticker[minvol$ticker == "SLBA"] <- "SLB"
minvol$ticker[minvol$ticker == "S*"] <- "S"
minvol$ticker[minvol$ticker == "SJR"] <- "SPN"
minvol$ticker[minvol$ticker == "T3W1"] <- "TWC"
minvol$ticker[minvol$ticker == "TWX*"] <- "TWX"
minvol$ticker[minvol$ticker == "TLK"] <- "TOL"
minvol$ticker[minvol$ticker == "TW6"] <- "TW"
minvol$ticker[minvol$ticker == "UUM"] <- "UNM"
minvol$ticker[minvol$ticker == "USX1"] <- "X"
minvol$ticker[minvol$ticker == "WFC*"] <- "WFC"
minvol$ticker[minvol$ticker == "MPN"] <- "MPC"
minvol$ticker[minvol$ticker == "H9B1"] <- "HTZ"
minvol$ticker[minvol$ticker == "NIHDQ"] <- "NIHD"
minvol$ticker[minvol$ticker == "PA9"] <- "TRV"
minvol$ticker[minvol$ticker == "UPLMQ"] <- "UPL"
minvol$ticker[minvol$ticker == "WLTGQ"] <- "WLT"

# From NYSE/NASDAQ info
minvol$ticker[minvol$ticker == "FCX*"] <- "FCX"
minvol$ticker[minvol$ticker == "UAC/C"] <- "UA"
minvol$ticker[minvol$ticker == "UAA"] <- "UA"
minvol$ticker[minvol$ticker == "BF/B"] <- "BF"

# Additional updates
minvol$ticker[minvol$ticker == "VISA"] <- "V"
minvol$ticker[minvol$ticker == "SPXC"] <- "SPW"
minvol$ticker[minvol$ticker == "GHC"] <- "WPO"
minvol$ticker[minvol$ticker == "LDOS"] <- "SAI"
minvol$ticker[minvol$ticker == "AAZ"] <- "APC"
minvol$ticker[minvol$ticker == "NCRA"] <- "NWS"
minvol$ticker[minvol$ticker == "ANTM"] <- "WLP"
minvol$ticker[minvol$ticker == "BRKB"] <- "BRK"

# KMI WS is a KINDER MORGAN EQUITY WARRANTS EXP --> removed
minvol <- filter(minvol, ticker != "KMI WS")

# Some tickers have NA values
# Will add tickers for the two by creating new data set, then removing the old data and rbinding it
X <- filter(minvol, name == "US STEEL CORP CORP")
X$ticker <- "X"

ANR <- filter(minvol, name == "ALPHA NATURAL RESOURCES INC")
ANR$ticker <- "ANR"

BF <- filter(minvol, name == "BROWN FORMAN CORP CLASS B")
BF$ticker <- "BF"
BF$ticker <- as.factor(BF$ticker)

NIHD <- filter(minvol, name == "NII HOLDINGS INC CLASS B")
NIHD$ticker <- "NIHD"
NIHD$ticker <- as.factor(NIHD$ticker)

UPL <- filter(minvol, name == "ULTRA PETROLEUM CORP")
UPL$ticker <- "UPL"
UPL$ticker <- as.factor(UPL$ticker)

WLT <- filter(minvol, name == "WALTER ENERGY INC")
WLT$ticker <- "WLT"
WLT$ticker <- as.factor(WLT$ticker)

WPO <- filter(minvol, name == "GRAHAM HOLDINGS COMPANY CO CLASS B")
WPO$ticker <- "WPO"
WPO$ticker <- as.factor(WPO$ticker)

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
minvol <- rbind(minvol, X)
minvol <- rbind(minvol, ANR)
minvol <- rbind(minvol, BF)
minvol <- rbind(minvol, NIHD)
minvol <- rbind(minvol, UPL)
minvol <- rbind(minvol, WLT)
minvol <- rbind(minvol, SAI)
minvol <- rbind(minvol, WPO)
minvol <- rbind(minvol, ANTM)
minvol <- rbind(minvol, WLP)
minvol <- rbind(minvol, BRK)

minvol$ticker <- as.factor(minvol$ticker)

# Arrange by ticker
minvol <- arrange(minvol, ticker)

# Remove ORCHARD SUPPLY HARDWARE STORES tickers (OSHSQ and OSHWQ)
minvol <- minvol[-c(26965, 26966), ]

# minvol data set has dates for 2017-01-05. Remove then
minvol <- filter(minvol, date != "2017-01-05")
minvol$date <- as.Date(minvol$date)

# Several companies change names or tickers midway through data set
# have to manually input them

# **LPT, LIBERTY PROPERTY REIT TRUST, change to LRY for dates up
# until 2014-01-31 (but leave as LPT for dates after that)
LPT <- filter(minvol, name == "LIBERTY PROPERTY REIT TRUST")
LPT$ticker <- "LPT"
LPT$ticker <- as.factor(LPT$ticker)
LPT$date <- as.Date(LPT$date)

LPT1 <- filter(LPT, date <= "2014-01-31")
LPT1$ticker <- "LRY"

LPT <- filter(LPT, date > "2014-01-31")

LPT <- rbind(LPT, LPT1)

minvol <- minvol[!(minvol$name=="LIBERTY PROPERTY REIT TRUST"),]

minvol <- rbind(minvol, LPT)


#**ZBH, ZIMMER BIOMET HOLDINGS INC, change to ZMH for dates up
# until 2014-11-28, but leave as is for dates after
ZBH <- filter(minvol, name == "ZIMMER BIOMET HOLDINGS INC")
ZBH$ticker <- "ZBH"
ZBH$ticker <- as.factor(ZBH$ticker)
ZBH$date <- as.Date(ZBH$date)

ZBH1 <- filter(ZBH, date <= "2014-11-28")
ZBH1$ticker <- "ZMH"

ZBH <- filter(ZBH, date > "2014-11-28")

ZBH <- rbind(ZBH, ZBH1)

minvol <- minvol[!(minvol$name=="ZIMMER BIOMET HOLDINGS INC"),]

minvol <- rbind(minvol, ZBH)


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

#**NYCB, NEW YORK COMMUNITY BANCORP INC
#change to NYB until 2012-10-31, then leave the same
NYCB <- filter(minvol, name == "NEW YORK COMMUNITY BANCORP INC")
NYCB$ticker <- "NYCB"
NYCB$ticker <- as.factor(NYCB$ticker)
NYCB$date <- as.Date(NYCB$date)

NYCB1 <- filter(NYCB, date <= "2012-10-31")
NYCB1$ticker <- "NYB"

NYCB <- filter(NYCB, date > "2012-10-31")

NYCB <- rbind(NYCB, NYCB1)

minvol <- minvol[!(minvol$name=="NEW YORK COMMUNITY BANCORP INC"),]

minvol <- rbind(minvol, NYCB)


#**EA, ELECTRONIC ARTS INC
# change to ERTS until 2011-11-30. Then leave as EA.
EA <- filter(minvol, name == "ELECTRONIC ARTS INC")
EA$ticker <- "EA"
EA$ticker <- as.factor(EA$ticker)
EA$date <- as.Date(EA$date)

EA1 <- filter(EA, date <= "2011-11-30")
EA1$ticker <- "ERTS"

EA <- filter(EA, date > "2011-11-30")

EA <- rbind(EA, EA1)

minvol <- minvol[!(minvol$name=="ELECTRONIC ARTS INC"),]

minvol <- rbind(minvol, EA)

# **QVCA, LIBERTY INTERACTIVE QVC GROUP CORP
# LIBERTY INTERACTIVE CORP SERIES A (do this for both)
# change to LINTA until 2014-09-30 then leave as it
QVCA <- filter(minvol, name == "LIBERTY INTERACTIVE QVC GROUP CORP" | name == "LIBERTY INTERACTIVE CORP SERIES A")
QVCA$ticker <- "QVCA"
QVCA$ticker <- as.factor(QVCA$ticker)
QVCA$date <- as.Date(QVCA$date)

QVCA1 <- filter(QVCA, date <= "2014-09-30")
QVCA1$ticker <- "LINTA"

QVCA <- filter(QVCA, date > "2014-09-30")

QVCA <- rbind(QVCA, QVCA1)

minvol <- minvol[!(minvol$name == "LIBERTY INTERACTIVE QVC GROUP CORP" | minvol$name == "LIBERTY INTERACTIVE CORP SERIES A"),]

minvol <- rbind(minvol, QVCA)

# **LB, L BRANDS INC
# change to LTD until 2013-11-29 then leave as is
LB <- filter(minvol, name == "L BRANDS INC")
LB$ticker <- "LB"
LB$ticker <- as.factor(LB$ticker)
LB$date <- as.Date(LB$date)

LB1 <- filter(LB, date <= "2013-11-29")
LB1$ticker <- "LTD"

LB <- filter(LB, date > "2013-11-29")

LB <- rbind(LB, LB1)

minvol <- minvol[!(minvol$name=="L BRANDS INC"),]

minvol <- rbind(minvol, LB)

# **ES, NORTHEAST UTILITIES
# change to NU until 2014-11-28, then leave be).
ES <- filter(minvol, name == "NORTHEAST UTILITIES")
ES$ticker <- "NU"
ES$ticker <- as.factor(ES$ticker)
ES$date <- as.Date(ES$date)

ES1 <- filter(ES, date <= "2014-11-28")
ES1$ticker <- "NU"

ES <- filter(ES, date > "2014-11-28")

ES <- rbind(ES, ES1)

minvol <- minvol[!(minvol$name=="NORTHEAST UTILITIES"),]

minvol <- rbind(minvol, ES)

# **JOY, JOY GLOBAL INC
# change to JOYG until 2011-11-30 then leave be
JOY <- filter(minvol, name == "JOY GLOBAL INC")
JOY$ticker <- "JOY"
JOY$ticker <- as.factor(JOY$ticker)
JOY$date <- as.Date(JOY$date)

JOY1 <- filter(JOY, date <= "2011-11-30")
JOY1$ticker <- "JOYG"

JOY <- filter(JOY, date > "2011-11-30")

JOY <- rbind(JOY, JOY1)

minvol <- minvol[!(minvol$name=="JOY GLOBAL INC"),]

minvol <- rbind(minvol, JOY)

#**VIAB, VIACOM INC CLASS B
# change to VIA up until 2011-11-30, then leave as is
VIAB <- filter(minvol, name == "VIACOM INC CLASS B")
VIAB$ticker <- "VIAB"
VIAB$ticker <- as.factor(VIAB$ticker)
VIAB$date <- as.Date(VIAB$date)

VIAB1 <- filter(VIAB, date <= "2011-11-30")
VIAB1$ticker <- "VIA"

VIAB <- filter(VIAB, date > "2011-11-30")

VIAB <- rbind(VIAB, VIAB1)

minvol <- minvol[!(minvol$name=="VIACOM INC CLASS B"),]

minvol <- rbind(minvol, VIAB)

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
# X6b23499486e8a31c <- read_csv("~/Downloads/X6b23499486e8a31c")
# minvol_prices <- X6b23499486e8a31c
colnames(minvol_prices) <- c("permco", "date", "ticker", "price")
minvol_prices <- select(minvol_prices, date, ticker, price)
minvol_prices$price <- abs(minvol_prices$price)
library(lubridate)
minvol_prices$date <- ymd(minvol_prices$date)
minvol_prices$ticker <- as.factor(minvol_prices$ticker)

# Merge with monthly_returns_minvol, by date and ticker
monthly_returns_minvol <- merge(monthly_returns_minvol, minvol_prices, by = c("date", "ticker"), all.x = TRUE)

# Arrange by ticker
monthly_returns_minvol <- arrange(monthly_returns_minvol, ticker)

# unique tickers
unique_tickers <- unique(minvol$ticker)
monthly_returns_minvol1 <- matrix(ncol=5)
colnames(monthly_returns_minvol1) <- c("date", "ticker", "weight", "price", "delta")
library(quantmod)
library(dplyr)

# Calculate the delta, change in return (monthly)
for (n in unique_tickers){
	# Test file to make sure enough observations are available
	temp <- filter(monthly_returns_minvol, ticker == n)
	temp <- temp[complete.cases(temp),]

	# Create different datasets by state. Make sure file has at least 252 non NA values
	if (nrow(temp) > 1){
		ticker_data <- filter(monthly_returns_minvol, ticker == n)
		ticker_data <- select(ticker_data, date, ticker, weight, price)
		ticker_data <- ticker_data[complete.cases(ticker_data),]
		ticker_data$delta <- Delt(ticker_data$price)
		ticker_data <- select(ticker_data, date, ticker, weight, price, delta)
	}
	else {
	}

	monthly_returns_minvol1 <- rbind(monthly_returns_minvol1, ticker_data)
	monthly_returns_minvol1$date <- as.Date(monthly_returns_minvol1$date)
}

# there are some NA values --> remove them
monthly_returns_minvol1 <- monthly_returns_minvol1[complete.cases(monthly_returns_minvol1),]

# Multiply returns by weight
monthly_returns_minvol1$weighted_return <- monthly_returns_minvol1$weight * monthly_returns_minvol1$delta

# Aggregate the returns based on date
minvol_returns <- aggregate(weighted_return ~ date, data=monthly_returns_minvol1, FUN=sum)

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

returns2 <- merge(minvol_returns, eminvol_return, by = "date")

# Remove 2011-10-31 for now, because many NAs
returns2 <- returns2[!(returns2$date=="2011-10-31"),]
returns2 <- returns2[!(returns2$date=="2011-11-30"),]

# Plot returns calculated individually vs returns from Eminvol index
df1<-data.frame(x=returns2$date,y=returns2$weighted_return)
df2<-data.frame(x=returns2$date,y=returns2$eminvol_return)

ggplot(returns2, aes(x=weighted_return, y=eminvol_return)) +
	geom_point(shape=1) +  geom_smooth(method=lm)

cor(returns2$eminvol_return, returns2$weighted_return)
