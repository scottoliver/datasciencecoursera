complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'id' is an integer vector indicating the monitor id numbers
  ## to be used
  
  ## Return a data frame of the form
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor id and 'nobs' is the 
  ## number of complete cases
  
  file_names <- list.files(directory) ## get all files in directory
  file_path <- file.path(directory, file_names[id]) ## path of monitor files requested
  
  nobs = vector(mode="numeric", length = length(id)) ## declare observations vector with length of IDs selected
  counter <- 1 ## declare counter
  
  for(x in id) { ## for each monitor 
    fileData <- read.csv(file_path[counter], header = TRUE) ## read CSV file
    fileData <- na.omit(fileData) ## omit NA values
    fileRows <- nrow(fileData) ## count number of rows with values
    nobs[counter] <- fileRows ## set observations in observations vector
    counter <- counter + 1 ## increase counter
  }
  
  return(data.frame(id, nobs)) ## return data frame of monitor IDs and number of observations
}