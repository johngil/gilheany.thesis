getPath_min_vol <- function(){
	source_link <- "https://www.ishares.com/us/products/239695/ishares-msci-usa-minimum-volatility-etf/1467271812596.ajax?fileType=csv&fileName=USMV_holdings&dataType=fund"
	url <- getwd()
	path <- paste(url, "temp2.csv", sep = "/")
	result = c(source_link, path)
	return(result);
}

# download_file_min_vol()
#
# Download the dataset from https://www.ishares.com
download_file_min_vol <- function(){
	src <- getPath_min_vol();
	message(src[2])
	download.file(src[1], destfile = src[2])
}

# printTable_min_vol()
#
# Print out the table of MSCI min volatility constituents for a specific date
printTable_min_vol <- function(){
	src <- getPath_min_vol();
	if (!file.exists(src[2])) message("You need to download the dataset (run download_file_min_vol)")
	else {
		all_content <- readLines(src[2])
		skip_ten = all_content[-c(1:10)]
		data2 = read.csv(textConnection(skip_ten), header = TRUE, stringsAsFactors = FALSE)
	}
}

# printDate_min_vol()
#
# Print out the date when the MSCI min_vol data printed out
# by printTable_min_vol() was updated

printDate_min_vol <- function(){
	src <- getPath_min_vol();
	all_content <- readLines(src[2])
	date1= all_content[c(3)]
	date = gsub(".*,","",date1)
	message(date)
}
