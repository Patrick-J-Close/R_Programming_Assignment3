##Read outcome data from .csv file
##Check that the inputs of "state" and "outcome are valid
##Return hospital name with lowest 30-day death rate

best <- function(state, outcome){
     #Read data source
     data_source <- "./outcome-of-care-measures.csv"
     outcome_data <- read.csv(data_source, colClasses = "character")
     
     #change data from type character to numeric
     outcome_data[,11] <- as.numeric(outcome_data[,11]) #heart attack col. 11
     outcome_data[,17] <- as.numeric(outcome_data[,17]) #heart failure col. 17
     outcome_data[,23] <- as.numeric(outcome_data[,23]) #pneumonia
     
     #validate inputs from user
     valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
     if (!outcome %in% valid_outcomes){
          stop("invalid outcome")
     }else if(!state %in% outcome_data$State){
          stop("invalid state")
     }else{
          if(outcome == "heart attack"){
               hosp_name <- find_hosp(outcome_data, 11, state)
          }else if(outcome == "heart failure"){
               hosp_name <- find_hosp(outcome_data, 17, state)
          }else if(outcome == "pneumonia"){
               hosp_name <- find_hosp(outcome_data, 23, state)
          }
          return(hosp_name)
     }
     
}


##Find hospital with lowest 30-day death rate
##given outcome -> col_num, state
find_hosp <- function(outcome_data, col_num, state){
     #create subset of states from col 7
     states_subset <- outcome_data[outcome_data[,7]==state, ]
     outcome_subset <- states_subset[, col_num]
     
     #find min of subset
     hosp_min <- min(outcome_subset, na.rm = T)
     min_row <- which(outcome_subset == hosp_min)
     hosp_name <- states_subset[min_row, 2]
     
     return(hosp_name)
}