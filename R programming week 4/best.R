outcomeOfCareMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospitalData <- read.csv("hospital-data.csv", colClasses = "character")

best <- function(state, outcome) {
  if (state %in% outcomeOfCareMeasures$State) {
    stateData <- subset.data.frame(outcomeOfCareMeasures, outcomeOfCareMeasures$State == state)
    
    if ("heart attack" %in% outcome) {
      indexOfMinimum <- which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      stateData$Hospital.Name[indexOfMinimum]
    }
    else if ("heart failure" %in% outcome) {
      indexOfMinimum <- which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      stateData$Hospital.Name[indexOfMinimum]
    }
    else if ("pneumonia" %in% outcome) {
      indexOfMinimum <- which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      stateData$Hospital.Name[indexOfMinimum]
    }
    else {
      stop("invalid outcome")
    }
  }
  else {
    stop("invalid state")
  }
}

best("AK", "pneumonia")


