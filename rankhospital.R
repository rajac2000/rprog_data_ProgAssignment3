## Ranking hospitals by outcome in a state
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate

rankhospital <- function(state= "AL", outcome= "heart attack", num) {

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
  if(is.numeric(num) & num <= 0)  {
      return("Invalid Ranking Number")
  }else if(is.character(num))  {
    if(num != "worst" & num !="best" & 
       num != "Worst" & num !="Best"){
      return("Invalid Ranking Case, use : 'worst' or  'best'")
    }
  }
  
  outcome_temp <- outcome_temp2[outcome_temp2$State == state, ]

  
  if (outcome == "heart attack"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            as.numeric(outcome_temp[,11]))
  }
  else if (outcome == "heart failure"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            as.numeric(outcome_temp[,17]))
  }
  else if (outcome == "pneumonia"){
    outcome_df = data.frame(outcome_temp[,02],
                            outcome_temp[,07],
                            as.numeric(outcome_temp[,23]))
  }
  colnames(outcome_df) <- c("Name","ST","Outcome")
  
  if (num == "best" | num == "Best"){
    SummMin <- as.vector(summary
                         (outcome_df[outcome_df$ST == state,"Outcome"])
                         ["Min."])
   
   r <- as.character(outcome_df[outcome_df$Outcome == SummMin &
              !is.na(outcome_df$Outcome) ,"Name"])
   head(sort(r),1)
  }else if(num == "worst" | num == "Worst"){
    SummMax <- as.vector(summary
                         (outcome_df[outcome_df$ST == state,"Outcome"])
                         ["Max."])
    
    r <- as.character(outcome_df[outcome_df$Outcome == SummMax &
                              !is.na(outcome_df$Outcome) ,"Name"])
    head(sort(r),1)
  }else{
    sorted_df <- outcome_df[order(outcome_df$Outcome, 
                                  outcome_df$Name),c(1,3)]
    as.character(sorted_df[num,1])
  }
}