rankall <- function(outcome, num = "best") {

  # Read outcome data.
  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings = c("Not Available"),
                   colClasses = "character")

  # Map outcome to column number; check that outcome is valid.
  c <- switch(outcome,
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=23,
    -1
  )
  if (c == -1) stop("invalid outcome")

  # Split data by state, grabbing hospital names and stats.
  state_data = split(data[,c(2,c,7)], data$State)

  # Operate on each state.
  hosp_state = lapply(state_data, function(x) {

    # Order on the outcome (and secondarily, on hospital name); remove NAs.
    vals <- as.numeric(x[,2])
    hnames <- x[,1]
    ordered <- x[order(vals,hnames),]
    ordered <- ordered[!is.na(ordered[,2]),]

    # Evaluate to name of ith-ranked hospital (or best, or worst), plus state.
    ordered_names <- ordered[,1]
    sname = x[,3][1]  # prob there is a better way to do this
    hname =
      if (num == "best") {
        ordered_names[1]
      } else if (num == "worst") {
        ordered_names[length(ordered_names)]
      } else {
        ordered_names[num]
      }
    c(hname, sname)
  })

  # Convert split data frame mapping state to hospital name and state into final
  # format, data frame (indexed by state) associating hospital name and state.
  m <- do.call("rbind", hosp_state)
  colnames(m) <- c("hospital", "state")
  as.data.frame(m)

}

# print(head(rankall("heart attack", 20), 10))
# print(tail(rankall("pneumonia", "worst"), 3))
# print(tail(rankall("heart failure"), 10))