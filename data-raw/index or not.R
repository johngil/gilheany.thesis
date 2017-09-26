# Make a duplicate data frame for monthly
monthly_test <- monthly5

# Subset minvol data for date, ticker, and weight
minvol_test <- select(minvol, ticker, date, weight)

# Merge date and ticker with minvol weight
monthly_test1 <- merge(monthly_test, minvol_test, by = c("date", "ticker"), all.x = TRUE)

# If NA means not in min vol index, replace with 0
monthly_test1$weight[is.na(monthly_test1$weight)] <- 0

# Create new row with 0 or 1 for weight (in min vol index NOW or not)
monthly_test1$index_now <- 0
monthly_test1$index_now[monthly_test1$weight!=0] <- 1
monthly_test1$index_now <- factor(monthly_test1$index_now)

# Create new row with 0 or 1 for weight (in min vol index BEFORE [6 months ago] or not)
monthly_test1$index_before <- 0
monthly_test2 <- monthly_test1
monthly_test2 <-
	monthly_test2 %>%
	group_by(ticker) %>%
	mutate(index_before = dplyr::lag(index_now, n = 6, default = NA))

monthly_final <- monthly_test2

#building the logit model
mylogit <- glm(index_now ~  volatility + beta + price_to_book + index_before, data = monthly_final, family = "binomial")
summary(mylogit)
confint(mylogit)


