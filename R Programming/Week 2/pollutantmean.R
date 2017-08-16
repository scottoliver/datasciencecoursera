pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor id numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list 
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: do not round the result
  
  file_names <- list.files(directory) ## get all files in directory
  file_path <- file.path(directory, file_names[id]) ## path of monitor files requested
  pollutantDataFrames <- lapply(file_path, read.csv) ## read all CSV files in file_path
  combinedDataFrame <- Reduce(function(x, y) rbind(x, y), pollutantDataFrames) ## combine CSVs into one data frame using row bind
  mean(combinedDataFrame[, pollutant], na.rm = TRUE) ## compute the mean
}