##Return hospital name for the given state for the given rank
##based on 30-day death rates
rankhospital <- function(state, outcome, num = "best"){
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
     }else if(num == "best"){
          hosp_name <- best(state, outcome)
     }else{
          if(outcome == "heart attack"){
               hosp_name <- find_hosp(outcome_data, 11, state, num)
          }else if(outcome == "heart failure"){
               hosp_name <- find_hosp(outcome_data, 17, state, num)
          }else if(outcome == "pneumonia"){
               hosp_name <- find_hosp(outcome_data, 23, state, num)
          }
          result <- hosp_name
          return(result)
     }
     
}

##function to find hospital given state and rank
find_hosp <- function(outcome_data, col_num, state, num){
     states_subset <- outcome_data[outcome_data[,7]==state,]
     outcome_subset <- states_subset[, col_num]
     
     #define length of array
     len <- dim(states_subset[!is.na(outcome_subset), ])[1]
     
     if(num == "worst"){
          hosp_name <- find_rank(states_subset, outcome_subset, len)
     }else if( num > len){
          hosp_name <- NA
     }else{
          hosp_name <- find_rank(states_subset, outcome_subset, num)
     }
     return(hosp_name)
}

##find hospital name for given rank
find_rank <- function(states_subset, outcome_subset, num){
     hosp_name <- states_subset[,2][order(outcome_subset, states_subset[,2])[num]]
     return(hosp_name)
}