corr <- function(directory, threshold = 0) {
  setwd(directory)
  listOfFiles <- list.files()

  completeCases <- complete(directory, 1:332)
  completeCasesWithTreshold <- completeCases[which(completeCases$nobs > threshold), ]
  
  ids <- completeCasesWithTreshold[,1]
  
  
  correlations <- vector()
  
  for(i in ids) {
    csvFile2 <- read.csv(listOfFiles[i])
    good <- complete.cases(csvFile2)
    completeCases2 <- csvFile2[good, ]
    

    
    sulfateColumn <- completeCases2[,2]
    sulfateColumnWithoutNA <- sulfateColumn[!is.na(sulfateColumn)]

    nitrateColumn <- completeCases2[,3]
    nitrateColumnWithoutNA <- nitrateColumn[!is.na(nitrateColumn)]
    

    correlation <- cor(nitrateColumnWithoutNA, sulfateColumnWithoutNA)

    correlations <- append(correlations, correlation)
  }
  correlations
  
}

cr <- corr("C:\\Users\\coen_\\OneDrive\\Bureaublad\\datasciencecoursera\\specdata", 2000)
n <- length(cr)                
cr <- corr("C:\\Users\\coen_\\OneDrive\\Bureaublad\\datasciencecoursera\\specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


complete <- function(directory, id = 1:332) {
  setwd(directory)
  listOfFiles <- list.files()
  outputDF <- data.frame(FALSE, FALSE)
  outputDF <- outputDF[FALSE,]
  
  
  for(i in id) {
    csvFile <- read.csv(listOfFiles[i])
    good <- complete.cases(csvFile)
    completeCases <- csvFile[good, ]
    completeCasesCount <- nrow(completeCases)
    newRow <- data.frame(id=i, nobs=completeCasesCount)
    outputDF <- rbind(outputDF, newRow)
  }
  outputDF
}