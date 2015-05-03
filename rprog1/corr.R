corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  fcomplete <- complete(directory)
  cr <- numeric()
  numtrs <- subset(fcomplete$id, fcomplete$nobs >= threshold)
  for (i in numtrs) {
    fcsv <- read.csv(file=sprintf("%s/%03d.csv", directory, i), header=TRUE)
    fcsv <- fcsv[complete.cases(fcsv),]
    cr <- c(cr, cor(fcsv$sulfate, fcsv$nitrate))
  }
  cr
}