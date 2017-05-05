\name{gilheany.thesis}
\alias{gilheany.thesis}

\title{The gilheany.thesis package}

\description{
This package includes USMV and EUSA MSCI data sets taken from www.ishares.com. Data was aggregated from 2011-10-31 and 2017-01-05 and includes monthly data analysis, which shows the month end trailing volatility, beta, market cap, book value, B/P for each ticker. Data from the last 10 years was also taken from WRDS to help calculate these values. The purpose of this package is to show the change in volatility metrics for each constituent of the both indices. This data will be used to create a model to forecast future constiuents of USMV.
}
\details{

\bold{minvol}

aggregate of ishares data from the MSCI Minimum Volatility index (USMV ) from 2011-10-31 and 2017-01-05. Includes data on "Ticker", "Name", "Asset.Class", "Weight", "Price", "Shares", "Market.Value", "Notional.Value", "Sector", "SEDOL", "ISIN", "Exchange" and "Date". Data is from https://www.ishares.com/us/products/239693/ishares-msci-usa-etf

\bold{usa}

aggregate of ishares data from the MSCI USA Equal Weight Index (EUSA) from 2011-10-31 and 2017-01-05. Includes data on "Ticker", "Name", "Asset.Class", "Weight", "Price", "Shares", "Market.Value", "Notional.Value", "Sector", "SEDOL", "ISIN", "Exchange" and "Date". Data is from https://www.ishares.com/us/products/239695/ishares-msci-usa-minimum-volatility-etf

\bold{monthly}

month end trailing volatility, beta, market cap, book value, B/P for each ticker. Dates range from 2008-01-31 to 2016-12-31. Data coming from monthly_rolling_vol and monthly_beta_values


}
