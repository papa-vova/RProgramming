rankall <- function(outcome, num = "best") {
  ## reading care measures data
  hospdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## setting valid outcomes and column names
  valid_outcomes <- data.frame(
    cout = c("heart attack", "heart failure", "pneumonia"),
    ccol = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  ## checking input parameters for validity
  if (!any(outcome == valid_outcomes$cout)) {
    stop("invalid outcome")
  }
  if(num != "best" & num != "worst" & !is.numeric(num)) { stop("invalid position") }
  
  # getting data on hospitals by state and valid outcome, sorting by rank
  cause <- as.character(valid_outcomes$ccol[valid_outcomes$cout==outcome])
  ordered_hospdata <- hospdata[, c("Hospital.Name", "State", cause)]
  ordered_hospdata <- ordered_hospdata[
    order(ordered_hospdata[,2],
          suppressWarnings(as.numeric(ordered_hospdata[,3])),
          ordered_hospdata[,1]),]
  # excluding hospitals for which the rank data is not available
  ordered_hospdata <- ordered_hospdata[ordered_hospdata[,3] !=
                                           "Not Available",]
  # selecting state hospitals according to desired rank
  getrank(ordered_hospdata, num)
}

getrank <- function(data, num = best) {
  hospitals <- character(0)
  states <- character(0)
  states_list <- unique(data$State)
  for(i in 1:length(states_list)) {
    state <- states_list[i]
    # selecting state data
    rankedstate <- data[data$State == state, c(1, 3)]
    # getting hospital name for the state according to requested rank
    pos <- num
    if(num == "best") { pos <- 1 }
    else if(num == "worst") { pos <- dim(rankedstate)[1] }
    hospitals[i] <- rankedstate[pos,1]
    states[i] <- state
  }
  ranked <- data.frame(hospital=hospitals,state=states)
  ranked
}