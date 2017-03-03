usa <- read.csv("inst/extdata/processed_data/usa/usa.csv")
minvol <- read.csv("inst/extdata/processed_data/minvol/minvol.csv")

devtools::use_data(usa,overwrite = TRUE)
devtools::use_data(minvol,overwrite = TRUE)

