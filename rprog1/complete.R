complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  fcc <- data.frame(id = numeric(), nobs = numeric())
  
  for(i in id) {
    fcsv <- read.csv(file=sprintf("%s/%03d.csv", directory, i), header=TRUE)
    fcc <- rbind(fcc, data.frame(id=i, nobs=nrow(fcsv[complete.cases(fcsv),])))
  }
  fcc
}