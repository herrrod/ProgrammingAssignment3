rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[data$State == state,]
  
  if (outcome == "heart attack") {
    data <- data[,c(2,11)]
  } else if (outcome == "heart failure") {
    data <- data[,c(2,17)]    
  } else if (outcome == "pneumonia") {
    data <- data[,c(2,23)]    
  } else {
    stop("invalid outcome")
  }    
  
  if (nrow(data) == 0) {
    stop("invalid state")
  }
  
  data[,2] <- as.numeric(data[,2])
  data <- data[!is.na(data[,2]),]
  data <- data[order(data[,2], data[,1]),]
  
  if (num == "best") {
    data[1,1]
  } else if (num == "worst") {
    tail(data, 1)[1,1]
  } else {
    if (num > nrow(data)) {
      print(NA)
    } else {
      data[num, 1]
    }
  }
}