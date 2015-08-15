pollutantmean <- function(directory, pollutant, id = 1:332) {

  adf <- data.frame()
  
  for (monitor in id) {
    
    adf <-rbind(adf, read.csv(sprintf("%s/%03i.csv", directory, monitor)))
  }
  
  clean_adf <- adf[!is.na(adf[,pollutant]),pollutant]
  
  print(mean(clean_adf))
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}

#pollutantmean("specdata", "sulfate", 1:10) ## [1] 4.064
#pollutantmean("specdata", "nitrate", 70:72) ## [1] 1.706
#pollutantmean("specdata", "nitrate", 23) ## [1] 1.281
