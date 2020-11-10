outcomeOfCareMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospitalData <- read.csv("hospital-data.csv", colClasses = "character")


rankhospital <- function(state, outcome, num) {
  if (state %in% outcomeOfCareMeasures$State) {
    stateData <- subset.data.frame(outcomeOfCareMeasures, outcomeOfCareMeasures$State == state)
    
    if ("heart attack" %in% outcome) {
      sortedStateData <- stateData[order(as.numeric(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), stateData$Hospital.Name), ]
      sortedHospitalNames <- sortedStateData$Hospital.Name
      
      if ("worst" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        q <- q[!is.na(q)]
        q <- which.max(q)
        print(sortedHospitalNames[q])
      }
      else if ("best" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        q <- q[!is.na(q)]
        q <- which.min(q)
        print(sortedHospitalNames[q])
      }
      else {
        sortedHospitalNames[num]
      }
    }
    else if ("heart failure" %in% outcome) {
      sortedStateData <- stateData[order(as.numeric(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), stateData$Hospital.Name), ]
      sortedHospitalNames <- sortedStateData$Hospital.Name
      
      if ("worst" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        q <- q[!is.na(q)]
        q <- which.max(q)
        print(sortedHospitalNames[q])
      }
      else if ("best" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        q <- q[!is.na(q)]
        q <- which.min(q)
        print(sortedHospitalNames[q])
      }
      else {
        sortedHospitalNames[num]
      }
    }
    else if ("pneumonia" %in% outcome) {
      sortedStateData <- stateData[order(as.numeric(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), stateData$Hospital.Name), ]
      sortedHospitalNames <- sortedStateData$Hospital.Name
      sortedHospitalNames[num]
      
      if ("worst" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        q <- q[!is.na(q)]
        q <- which.max(q)
        print(sortedHospitalNames[q])
      }
      else if ("best" %in% num) {
        q <- as.numeric(sortedStateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        q <- q[!is.na(q)]
        q <- which.min(q)
        print(sortedHospitalNames[q])
      }
      else {
        sortedHospitalNames[num]
      }
    }
        
      
    else {
      stop("invalid outcome")
    }
  }
  else {
    stop("invalid state")
  }
}

rankhospital("NY", "heart attack", 7)