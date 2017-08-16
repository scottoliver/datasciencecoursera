corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all 
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  correlations = c() ## declare correlations vector
  
  file_names <- list.files(directory) ## get all files in directory
  file_path <- file.path(directory, file_names) ## path of all monitor files
  
  for (monitorFile in file_path) { ## for each monitor file
    monitorData <- read.csv(file = monitorFile, header = TRUE) ## read monitor data
    monitorData <- monitorData[complete.cases(monitorData), ] ## find complete cases in monitor data
    if (nrow(monitorData) > threshold) { ## if number of complete cases is greater than the threshold
      correlations <- c(correlations, cor(monitorData[, "nitrate"], monitorData[, "sulfate"])) ## add the correlation to the correlation vector
    }
  }
  
  return(correlations) ## return the correlations vector
  
}