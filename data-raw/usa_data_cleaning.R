
View(usa)
library(dplyr)
# Get rid of cash and the weird companies with numbers that have no ticker
usa1 <- usa %>% filter(complete.cases(Ticker))

#Create monthly data set with just the tickers and dates from the usa data
month_data <- select(usa1, Ticker, Date)
month_data <- arrange(month_data, Ticker, Date)

# I see some numeric tickers that should be removed. A60, 6COP, etc. Will find them all:
# Subset to exchanges not NYSE and NASDAQ
nonusa_ex <- filter(usa, Exchange != "New York Stock Exchange Inc.", Exchange != "NASDAQ")

# I did a check for the data with the two exchanges above, and data is perfect

# Find unique tickers in this nonusa_ex data set
library(tidyr)
unique(nonusa_ex$Ticker)

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

# Fill in values for vol, beta


# Fill in for P/B. Lag 3 months
