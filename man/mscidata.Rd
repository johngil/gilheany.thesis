\name{mscidata}
\alias{mscidata}

\title{The mscidata package}

\description{
mscidata data sets
}
\details{

\bold{minvol}

aggregate of ishares data from the MSCI Minimum Volatility index (USMV	) from 2011-10-31 and 2017-01-05. Includes data on "Ticker", "Name", "Asset.Class", "Weight", "Price", "Shares", "Market.Value", "Notional.Value", "Sector", "SEDOL", "ISIN", "Exchange" and "Date"
 
\bold{usa}

aggregate of ishares data from the MSCI USA Equal Weight Index (EUSA) from 2011-10-31 and 2017-01-05. Includes data on "Ticker", "Name", "Asset.Class", "Weight", "Price", "Shares", "Market.Value", "Notional.Value", "Sector", "SEDOL", "ISIN", "Exchange" and "Date"

\bold{minvol_percent} 

includes data calculations for the percent of each sector that comprises the stocks USMV index from 2011-10-31 and 2017-01-05. Sectors include Consumer Discretionary, Consumer Staples, Energy, Financials, Health Care, Industrials, Information Technology, Materials, Real Estate, S-T Securities, Telecommunications and Utilities. 

\bold{usa_percent}  

includes data calculations for the percent of each sector that comprises the stocks EUSA index from 2011-10-31 and 2017-01-05. Sectors include Consumer Discretionary, Consumer Staples, Energy, Financials, Health Care, Industrials, Information Technology, Materials, Real Estate, S-T Securities, Telecommunications and Utilities.

\bold{final_rolling_vol}  

calculations for the 1-year (252-day) rolling volatility calculations for each constituent of EUSA, using daily data from WRDS from 2006-12-31 to 2016-12-31, using the runSD function in the TTR library. Around 1.7M rows of data

\bold{monthly_rolling_vol}  

subset of final_rolling_vol that outputs the rolling volatilities of each ticker for the end of the month. 

\bold{wrds_data}  

data from WRDS that includes date, ticker and price of the EUSA constituents from 2006-12-31 to 2016-12-31

\bold{beta_calcs}  

percentage change in the daily price of each EUSA ticker
 
\bold{wrds_data_beta} 

Daily data from WRDS for 10 years with date, ticker, price, S&P 500 return, Delta (percent change in the stock price [from beta_calcs]), covariance of S&P returns and stock returns, variance of S&P 500 returns, and daily 1-year (252-day) rolling beta of each stock. 

\bold{beta_values} 

Beta calculations for all tickers for 10 years with Date, Ticker, Cov, Var, and Beta Values. Beta was calculated by finding the covariance and dividing by the variance

\bold{monthly_beta_values}  

month end Beta values for all tickers from 2008-01-31 to 2016-12-31. Data is taken from beta_values, which has the daily beta calculations 

\bold{monthly_data} 

month end trailing volatility and beta for each ticker. Dates range from 2008-01-31 to 2016-12-31. Data coming from monthly_rolling_vol and monthly_beta_values

\bold{annual_gics} 

industry information for each ticker from 2007-2016, including GIC Group, GIC sub-industry, GIC industry and GIC Sector

\bold{book_value_data} 

contains 10 years of data from WRDS, including "gvkey" , "Date" , "Year", "Ticker", "Total_Assets", "BV_per_share", "Shares_Outstanding", "Total_Liabilities", "Market_Value", "Share_Price" , "Book_Value" and the price to book ratio calculated in two different ways to ensure accuracy of the data
 
}