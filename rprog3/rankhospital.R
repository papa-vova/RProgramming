rankhospital <- function(state, outcome, num = "best") {
  ## reading care measures data
  hospdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(hospdata$State)
  
  ## setting valid outcomes and column names
  valid_outcomes <- data.frame(
    cout = c("heart attack", "heart failure", "pneumonia"),
    ccol = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  ## checking input parameters for validity
  if (!any(states == state)) { stop("invalid state") }
  if (!any(outcome == valid_outcomes$cout)) {
    stop("invalid outcome")
  }

  # getting data on hospitals by state and valid outcome
  cause <- as.character(valid_outcomes$ccol[valid_outcomes$cout==outcome])
  selected_hospdata <-
    hospdata[hospdata$State == state,][, c("Hospital.Name", cause)]
  selected_hospdata <- selected_hospdata[
    order(suppressWarnings(as.numeric(selected_hospdata[,2])),
          selected_hospdata[,1]),]
  # excluding hospitals for which the data is not available
  selected_hospdata <- selected_hospdata[selected_hospdata[,2] !=
                                           "Not Available",]
  # setting position according to desired rank
  pos <- num
  if(num == "best") { pos <- 1 }
  else if(num == "worst") { pos <- dim(selected_hospdata)[1] } 
  else if(!is.numeric(num)) { stop("invalid position") }
  selected_hospdata[pos,1]
}