best <- function(state, outcome) {
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if(outcome == valid_outcomes[1]){ ## heart attack
                datacolumn <- 11
        }
        if(outcome == valid_outcomes[2]){ ## heart failure
                datacolumn <- 17
        }
        if(outcome == valid_outcomes[3]){ ## pneumonia
                datacolumn <- 23
        }
        
        get_hospital_name(datasource, state, datacolumn)
        
}

## function retrieves the hospital names from the data source
get_hospital_name <- function(datasource, state, datacolumn){
        ## filter by state
        state_subset <- datasource[datasource$State == state, ] 
        
        ## make outcome column numeric
        suppressWarnings(state_subset[, datacolumn] <- as.numeric(
                state_subset[, datacolumn]))
        
        ## find minimum value
        min_value <- min(state_subset[, datacolumn], na.rm = TRUE)
        
        ## find rows with minimum value
        min_rownums <- which(state_subset[, datacolumn] == min_value)
        
        ## retreive names of best hospitals
        best_hospitals <- state_subset[min_rownums, 2]
        
        ## display the best hospitals
        print(best_hospitals)
}