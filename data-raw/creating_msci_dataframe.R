library(dplyr)
# Select data frame skeleton from usa with columsn we want
MSCI <- select(usa, date, ticker, name, weight, sector)
colnames(MSCI)[4] <- "usa.weight"

# Subset minvol data set into name, ticker, date, weight
minvol_msci <- select(minvol, date, ticker, name, weight)
colnames(minvol_msci)[4] <- "minvol.weight"

# Merge with min vol data set by name, ticker, and date
MSCI <- merge(MSCI, minvol_msci, by = c("date", "ticker", "name"), all.x = TRUE)

# Ensure data has been properly merged. Find number of NAs in minvol weight.
# Should be equal to 38598 - 9229
sum(is.na(MSCI$minvol.weight))

# Make na values in minvol.weight equal to 0
MSCI[is.na(MSCI)] <- 0.0000

# Reorder columns
MSCI <- MSCI[c(1,2,3,5,4,6)]



