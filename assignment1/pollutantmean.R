## POLLUTANT MEAN
##
## Returns the mean of the pollutant across all monitors listed
## in the 'id' vector (ignoring NA values).
##
pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- dir(directory, full.names = TRUE)[id]
  rows <- lapply(filenames, read.csv, header = TRUE)
  df <- do.call(rbind,rows)
  readings <- df[,pollutant]
  mean(readings, na.rm = TRUE)
}
