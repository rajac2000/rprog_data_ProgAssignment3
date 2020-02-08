## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
rankall <- function(outcome= "heart attack",  num = "best") {
  outcome_temp2 <- read.csv("./Data/outcome-of-care-measures.csv", 
                colClasses = "character")
  unsorted_states_list <- unique(outcome_temp2[,"State"])
  states_list <- sort(unsorted_states_list)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

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
  result_df <- data.frame(matrix(vector(), 0, 2))
  for(i in seq_along(states_list)){
    outcome_temp <- outcome_temp2[outcome_temp2$State == states_list[i], ]

    if (outcome == "heart attack"){
      outcome_df = data.frame(outcome_temp[,02],outcome_temp[,07],
                              as.numeric(outcome_temp[,11]))
    }
    else if (outcome == "heart failure"){
      outcome_df = data.frame(outcome_temp[,02],outcome_temp[,07],
                              as.numeric(outcome_temp[,17]))
    }
    else if (outcome == "pneumonia"){
      outcome_df = data.frame(outcome_temp[,02],outcome_temp[,07],
                              as.numeric(outcome_temp[,23]))
    }
    colnames(outcome_df) <- c("Name","ST","Outcome")
    if (num == "best" | num == "Best"){
      SummMin <- as.vector(summary
                        (outcome_df[outcome_df$ST == states_list[i],"Outcome"])
                        ["Min."])
      namevar <- head(sort(as.character(outcome_df[outcome_df$Outcome == SummMin &
                                !is.na(outcome_df$Outcome) ,"Name"])),1)
    }else if(num == "worst" | num == "Worst"){
      SummMax <- as.vector(summary
                        (outcome_df[outcome_df$ST == states_list[i],"Outcome"])
                        ["Max."])
      namevar <- head(sort(as.character(outcome_df[outcome_df$Outcome == SummMax &
                                !is.na(outcome_df$Outcome) ,"Name"])),1)
    }else{
      sorted_df <- outcome_df[order(outcome_df$Outcome, 
                                    outcome_df$Name),c(1,3)]
      namevar <- as.character(sorted_df[num,1])
    }
    temp_df   <- data.frame(namevar,states_list[i])
    result_df <- rbind(result_df,temp_df)
  }
  colnames(result_df) <- c("HOSPITAL","STATE")
  result_df
}