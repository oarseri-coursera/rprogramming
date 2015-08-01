rankhospital <- function(state, outcome, num = "best") {

  # Read outcome data.
  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings = c("Not Available"),
                   colClasses = "character")

  # Filter data by state; check that state is valid.
  state_data <- data[data$State == state,]
  if (nrow(state_data) < 1) stop("invalid state")

  # Map outcome to column number; check that outcome is valid.
  c <- switch(outcome,
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=23,
    -1
  )
  if (c == -1) stop("invalid outcome")

  # Order on the outcome (and secondarily, on hospital name); remove NAs
  vals <- as.numeric(state_data[,c])
  hnames <- state_data$Hospital.Name
  ordered <- state_data[order(vals,hnames),]
  ordered <- ordered[!is.na(ordered[,c]),]

  # Output name of ith-ranked hospital (or best, or worst).
  ordered_names <- ordered$Hospital.Name
  if (num == "best") {
    ordered_names[1]
  } else if (num == "worst") {
    ordered_names[length(ordered_names)]
  } else {
    ordered_names[num]
  }

}

# print(rankhospital("TX", "heart failure", 4))
# print(rankhospital("MD", "heart attack", "worst"))
# print(rankhospital("MN", "heart attack", 5000))
