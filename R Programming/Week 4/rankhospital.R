rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        datasource <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                               colClasses = "character")
        
        ## Check that state and outcome are valid
        uniqueStates <- unique(datasource$State)
        if(!(state %in% uniqueStates)){
                stop("Invalid state.")
        }
        
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% valid_outcomes)){
                stop("Invalid outcome.")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(outcome == valid_outcomes[1]){ ## heart attack
                datacolumn <- 11
        }
        if(outcome == valid_outcomes[2]){ ## heart failure
                datacolumn <- 17
        }
        if(outcome == valid_outcomes[3]){ ## pneumonia
                datacolumn <- 23
        }
        
        get_ranked_hospital(datasource, state, datacolumn, num)
        
}

get_ranked_hospital <- function(datasource, state, datacolumn, num){
        ##get subset for state
        state_subset <- datasource[datasource$State == state, ] 
        
        ## make outcome column numeric
        suppressWarnings(state_subset[, datacolumn] <- as.numeric(
                state_subset[, datacolumn]))
        
        ## remove NA values from datacolumn
        state_subset <- state_subset[!is.na(state_subset[, 11]), ]
        
        ##sort subset by appropriate column and hospital name
        sorted_state_subset <- state_subset[order(state_subset[, datacolumn], 
                                                  state_subset[, 2]), ]
        
        ##determine what to do with num
        if(num == "best"){
                rank <- 1
        }
        else if(num == "worst"){
                rank <- nrow(sorted_state_subset)
        }
        else{
                rank <- as.numeric(num)
                if(is.na(rank)){
                        stop("num is not a valid entry")
                }
                else if(rank > nrow(sorted_state_subset)){
                        return("NA")
                }
        }
        
        ##print name of hospital at rank number
        print(sorted_state_subset[rank, 2])
}