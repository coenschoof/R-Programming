if ("heart attack" %in% outcome) {
        sortedData <- outcomeOfCareMeasures[order(as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcomeOfCareMeasures$Hospital.Name), ]
        sortedHospitalNames <- sortedData$Hospital.Name
        
        if ("worst" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                q <- q[!is.na(q)]
                cbind(hospital = sortedHospitalNames, state = sortedData$State)
        }
        else if ("best" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                q <- q[!is.na(q)]
                q <- which.min(q)
                print(sortedHospitalNames[q])
        }
        else {
                d <- cbind(hospital = sortedHospitalNames, state = sortedData$State)
                
                
        }
}
else if ("heart failure" %in% outcome) {
        sortedData <- outcomeOfCareMeasures[order(as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), outcomeOfCareMeasures$Hospital.Name), ]
        sortedHospitalNames <- sortedData$Hospital.Name
        
        if ("worst" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                q <- q[!is.na(q)]
                cbind(hospital = sortedHospitalNames, state = sortedData$State)
        }
        else if ("best" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                q <- q[!is.na(q)]
                q <- which.min(q)
                print(sortedHospitalNames[q])
        }
        else {
                d <- cbind(sortedHospitalNames, as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                print(d)
        }
}
else if ("pneumonia" %in% outcome) {
        sortedData <- outcomeOfCareMeasures[order(as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcomeOfCareMeasures$Hospital.Name), ]
        sortedHospitalNames <- sortedData$Hospital.Name
        
        if ("worst" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                q <- q[!is.na(q)]
                cbind(hospital = sortedHospitalNames, state = sortedData$State)
        }
        else if ("best" %in% num) {
                q <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                q <- q[!is.na(q)]
                q <- which.min(q)
                print(sortedHospitalNames[q])
        }
        else {
                d <- cbind(sortedHospitalNames, as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                print(d)
        }
}
else {
        stop("invalid outcome")
}