best <- function(state, outcome) {

  # Read outcome data.
  #data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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

  # Get minimum value for outcome.
  vals <- as.numeric(state_data[,c])
  hnames <- state_data$Hospital.Name
  ordered <- state_data[order(vals,hnames),]

  ordered$Hospital.Name[1]
}


# print(list(
#   best("TX", "heart attack"),
#   best("TX", "heart failure"),
#   best("MD", "heart attack"),
#   best("MD", "pneumonia")
# ))

# best("BB", "heart attack")
# best("NY", "hert attack")
