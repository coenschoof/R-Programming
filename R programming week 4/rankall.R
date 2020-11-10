outcomeOfCareMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospitalData <- read.csv("hospital-data.csv", colClasses = "character")


rankall <- function(outcome, num) {
        outcomeOfCareMeasures <- outcomeOfCareMeasures[!is.na(as.numeric(outcomeOfCareMeasures[, 11])), ]
        listOfStates <- sort(outcomeOfCareMeasures[,7])
        ulistOfStates <- unique(listOfStates)
        
        sortedData <- outcomeOfCareMeasures[order(outcomeOfCareMeasures$State, as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
        orderedOutcomes <- sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        
        output <- split(sortedData$Hospital.Name, listOfStates)
        empty_list <- c()
        
        if ("worst" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[length(i)])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else if ("best" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[1])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else {
                empty_list <- c()
                
                for(i in output) {
                        empty_list <- c(empty_list, i[num])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }


        
        
}

rankall2 <- function(outcome, num = 1) {
        outcomeOfCareMeasures <- outcomeOfCareMeasures[!is.na(as.numeric(outcomeOfCareMeasures[, 17])), ]
        listOfStates <- sort(outcomeOfCareMeasures[,7])
        ulistOfStates <- unique(listOfStates)
        
        sortedData <- outcomeOfCareMeasures[order(outcomeOfCareMeasures$State, as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
        orderedOutcomes <- sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        
        output <- split(sortedData$Hospital.Name, listOfStates)
        empty_list <- c()
        
        if ("worst" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[length(i)])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else if ("best" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[1])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else {
                empty_list <- c()
                
                for(i in output) {
                        empty_list <- c(empty_list, i[num])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        
        
        
        
}

rankall3 <- function(outcome, num) {
        outcomeOfCareMeasures <- outcomeOfCareMeasures[!is.na(as.numeric(outcomeOfCareMeasures[, 23])), ]
        listOfStates <- sort(outcomeOfCareMeasures[,7])
        ulistOfStates <- unique(listOfStates)
        
        sortedData <- outcomeOfCareMeasures[order(outcomeOfCareMeasures$State, as.numeric(outcomeOfCareMeasures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
        orderedOutcomes <- sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        
        output <- split(sortedData$Hospital.Name, listOfStates)
        empty_list <- c()
        
        if ("worst" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[length(i)])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else if ("best" %in% num) {
                for(i in output) {
                        empty_list <- c(empty_list, i[1])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        else {
                empty_list <- c()
                
                for(i in output) {
                        empty_list <- c(empty_list, i[num])
                }
                
                cbind(hospital = empty_list, state = ulistOfStates)
        }
        
        
        
        
}


#Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
#Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure

#rankall("heart attack", 4)
#as.character(subset(r, state == "HI")$hospital)
#rankall3("pneumonia", "worst")
rankall2("heart failure", 10)


