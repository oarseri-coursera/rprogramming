## CORR
##
## Returns a numeric vector of correlations.
##
corr <- function(directory, threshold = 0) {
  filenames <- dir(directory, full.names = TRUE)
  files_to_rows <- lapply(filenames, read.csv, header = TRUE)
  nobs <- lapply(files_to_rows,
                 function(rows) sum(complete.cases(rows)))
  adequate_files <- nobs > threshold
  files_to_rows <- files_to_rows[adequate_files]
  corrs <- unlist(lapply(files_to_rows,
                         function(d) { df <- data.frame(d);
                                       cor(df[,"nitrate"], df[,"sulfate"],
                                           use="complete.obs") }))
  if (length(corrs) > 0)
    corrs
  else
    numeric()
}

