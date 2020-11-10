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
  print(outputDF)
  
}

cc <- complete("C:\\Users\\coen_\\OneDrive\\Bureaublad\\datasciencecoursera\\specdata", 332:1)
RNGversion("3.5.1")  
set.seed(42)
use <- sample(332, 10)
print(cc[use, "nobs"])