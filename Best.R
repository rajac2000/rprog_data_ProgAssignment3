## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate

best <- function(state= "AL", outcome= "heart attack") {

  outcome_temp2 <- read.csv("./Data/outcome-of-care-measures.csv", 
                colClasses = "character")

  states_list <- unique(outcome_temp2[,"State"])
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(state %in% states_list)) {
    return("Invalid State name")
  }
                                         
  if(!(outcome %in% valid_outcomes)) {
    return("Invalid outcome")
  }
  
  outcome_temp <- outcome_temp2[outcome_temp2$State == state, ]
  
  outcome_temp[,11] <- as.numeric(outcome_temp[,11])
  outcome_temp[,17] <- as.numeric(outcome_temp[,17])
  outcome_temp[,23] <- as.numeric(outcome_temp[,23])
  
  
  if (outcome == "heart attack"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            outcome_temp[,11])
  }
  else if (outcome == "heart failure"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            outcome_temp[,17])
  }
  else if (outcome == "pneumonia"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            outcome_temp[,23])
  }
  colnames(outcome_df) <- c("Name","ST","Outcome")
  SummMin <- as.vector(summary
                        (outcome_df[outcome_df$ST == state,"Outcome"])
                       ["Min."])
  
  as.character(outcome_df[outcome_df$Outcome == SummMin &
             !is.na(outcome_df$Outcome) ,"Name"])

}