pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd(directory)
  listOfFiles <- list.files()
  
  vec <- vector()

  for(i in id) {
    csvFile <- read.csv(listOfFiles[i])
    pollutantColumn <- csvFile[,pollutant]
    pollutantColumnWithoutNA <- pollutantColumn[!is.na(pollutantColumn)]
    
    vec <- append(vec, pollutantColumnWithoutNA)
    
  }

  mean(vec)
}


pollutantmean("C:\\Users\\coen_\\OneDrive\\Bureaublad\\datasciencecoursera\\specdata", "nitrate")