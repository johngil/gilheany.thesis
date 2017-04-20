getPath_usa <- function(){
	source_link <- "https://www.ishares.com/us/products/239693/ishares-msci-usa-etf/1467271812596.ajax?fileType=csv&fileName=EUSA_holdings&dataType=fund"
	url <- getwd()
	path <- paste(url, "temp2.csv", sep = "/")
	result = c(source_link, path)
	return(result);
}

# download_file_usa()
#
# Download the dataset from https://www.ishares.com
download_file_usa <- function(){
	src <- getPath_usa();
	download.file(src[1], destfile = src[2])
}

# printTable_usa()
#
# Print out the table of MSCI USA constituents for a specific date
printTable_usa <- function(){
	src <- getPath_usa();
	if (!file.exists(src[2])) stop("You need to download the dataset (run download_file_usa)")
	else {
		all_content <- readLines(src[2])
		skip_ten = all_content[-c(1:10)]
		data2 = read.csv(textConnection(skip_ten), header = TRUE, stringsAsFactors = FALSE)
		View(data2)
	}
}

# printDate_usa()
#
# Print out the date when the MSCI USA data printed out
# by printTable_usa() was updated
printDate_usa <- function(){
	src <- getPath_usa();
	all_content <- readLines(src[2])
	date1= all_content[c(3)]
	date = gsub(".*,","",date1)
	message(date)
}

