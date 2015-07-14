## COMPLETE
##
## Returns a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases.
##
complete <- function(directory, id = 1:332) {
  filenames <- dir(directory, full.names = TRUE)[id]
  files_to_rows <- lapply(filenames, read.csv, header = TRUE)
  nobs <- unlist(lapply(files_to_rows,
                        function(rows) sum(complete.cases(rows))))
  data.frame(id,nobs)
}
