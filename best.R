best <- function (state, outcome){
  data <- read.csv("outcome-of-care-measures.csv")
  A <- data$State == state
  ## Test if the variable: state is in the list of data$State
  ## If not, the sum of A will be 0.
  if (!sum(A)) {
    stop ("invalid state")
  }
  
  disease_list <- c("heart attack", "heart failure", "pneumonia")
  ## Test if the variable: outcome is in the list of disease
  if (!outcome %in% disease_list){
    stop ("invalid outcome")
  }
  ## Create the sub-data.frame for the specific state.
  StateData <- subset(data, State == state)
  ## Extract the hospital and rate colume from the data.
  if (outcome == "heart attack") { 
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  }
  else if (outcome == "heart failure"){
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  }
  ##  else if (outcome == "pneumonia")
  else {
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  }
  
  ## Assign a common colume name for the StateData whatever the disease is.
  colnames(StateData) <- c("Hospital.Name", "Disease.Rate")
  
  ## Transform the disease.rate colume from Factor to numeric for the purpose of ordering. 
  StateData[, "Disease.Rate"] <- as.numeric(as.character(StateData[, "Disease.Rate"]))
  StateData[, "Hospital.Name"] <- as.character(StateData[, "Hospital.Name"])
 
  ## Order the data.frame by disease rate and hospital names.
  StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ]
  best <- StateData[1, "Hospital.Name"]
  best
}
