# Make a duplicate data frame for monthly
monthly_test <- monthly

# Subset minvol data for date, ticker, and weight
minvol_test <- select(minvol, ticker, date, weight)

# Merge date and ticker with minvol weight
monthly_test1 <- merge(monthly_test, minvol_test, by = c("date", "ticker"), all.x = TRUE)

# If NA means not in min vol index, replace with 0
monthly_test1$weight[is.na(monthly_test1$weight)] <- 0

# Create new row with 0 or 1 for weight (in min vol index or not)
monthly_test1$binary <- 0
monthly_test1$binary[monthly_test1$weight!=0] <- 1
monthly_test1$binary <- factor(monthly_test1$binary)

#building the logit model
mylogit <- glm(binary ~  beta + volatility + market_value + price_to_book, data = monthly_test1, family = "binomial")
summary(mylogit)
confint(mylogit)


