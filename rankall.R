##
rankall <- function(outcome, num = "best"){
     
     #Read data source
     data_source <- "./outcome-of-care-measures.csv"
     outcome_data <- read.csv(data_source, colClasses = "character")
     
     state_arry <- sort(unique(outcome_data$State))
     len_arry <- length(state_arry)
     hospitals <- rep("", len_arry)
     
     #validate inputs from user
     valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
     if (!outcome %in% valid_outcomes){
          stop("invalid outcome")
     }else{
          for(i in 1:len_arry){
               states_subset <- outcome_data[outcome_data[,7]==state_arry[i],]
               if(outcome == "heart attack"){
                    hospitals[i] <- find_hosp(states_subset, 11, num)
               }else if(outcome == "heart failure"){
                    hospitals[i] <- find_hosp(states_subset, 17, num)
               }else if(outcome == "pneumonia"){
                    hospitals[i] <- find_hosp(states_subset, 23, num)
               }
          }
     }
     df <- data.frame(hospital=hospitals, state=state_arry)
     return(df)
}

##function to find hospital given state and rank
find_hosp <- function(states_subset, col_num, num){

     outcome_arry <- as.numeric(states_subset[, col_num])
     
     #define length of array
     len <- dim(states_subset[!is.na(outcome_arry), ])[1]
     
     if(num == "worst"){
          hosp_name <- find_rank(states_subset, outcome_arry, len)
     }else if(num == "best"){
          hosp_name <- find_rank(states_subset, outcome_arry, 1)
     }else if( num > len){
          hosp_name <- NA
     }else{
          hosp_name <- find_rank(states_subset, outcome_arry, num)
     }
     return(hosp_name)
}

##find hospital name for given rank
find_rank <- function(states_subset, outcome_arry, num){
     hosp_name <- states_subset[,2][order(outcome_arry, states_subset[,2])[num]]
     return(hosp_name)
}