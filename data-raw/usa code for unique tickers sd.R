library(dplyr)
library(stats)

data(usa)

unique(usa$Ticker)
table(usa$Ticker)


#usa1 <- usa %>%
#					group_by(Ticker) %>%
#					tq_mutate_xy(x = Price, mutate_fun = runSD, col_rename = "rollingSD")

usa_tickers_rolling_sd <- usa %>%
		group_by(Ticker) %>%
		mutate(SD = rollingSD(Price,h = min(n(),252)))

devtools::use_data(usa_tickers_rolling_sd)





