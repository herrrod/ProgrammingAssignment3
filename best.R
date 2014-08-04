
best <- function(state, outcome) {
  ## Read outcome data

  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[data$State == state,]
  
  if (outcome == "heart attack") {
    data <- data[,c(2,11)]
  } else if (outcome == "heart failure") {
    data <- data[,c(2,17)]    
  } else if (outcome == "pneumonia") {
    data <- data[,c(2,23)]    
  } else {
    print("Outcome not recognised")
  }    
  
  data[,2] <- as.numeric(data[,2])
  data <- data[!is.na(data[,2]),]
  
  ## indicies that are minimum, if >1 then 
  ## choose the first hospital alphabetically
  data <- data[data[,2] == min(data[,2])]
  data <- data[order(data[,1]),]
  data[1,1]
}
