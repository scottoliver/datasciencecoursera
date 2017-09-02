rankall <- function(outcome, num = "best") {
        ## Read outcome data
        datasource <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                               colClasses = "character")
        
        ## Check that state and outcome are valid
        
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% valid_outcomes)){
                stop("Invalid outcome.")
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ## Assign data column
        if(outcome == valid_outcomes[1]){ ## heart attack
                datacolumn <- 11
                datacolumn_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }
        if(outcome == valid_outcomes[2]){ ## heart failure
                datacolumn <- 17
                datacolumn_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }
        if(outcome == valid_outcomes[3]){ ## pneumonia
                datacolumn <- 23
                datacolumn_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        ## set data column as numeric
        datasource[, datacolumn] <- as.numeric(datasource[, datacolumn])
        
        ## Create subset of data for outcome
        subset_data <- datasource[, c("Hospital.Name", "State", datacolumn_name)]
        
        ## Split selected outcome data by state
        subset_data_by_state <- split(subset_data, subset_data$State)
        
        ## For each state, find the hospital of the given rank
        
        ## Use get_hospital_of_rank function and apply to each state to determine
        ## hospital of rank requested for each state
        
        get_hospital_of_rank <- function(data_by_state, num){
                
                ## Sort data by state
                ordered_data_by_state <- order(data_by_state[3], 
                                               data_by_state$Hospital.Name, na.last = NA)
                
                ## Determine what to do with num
                
                if(num == "best"){
                        data_by_state$Hospital.Name[ordered_data_by_state[1]]
                }
                else if(num == "worst"){
                        data_by_state$Hospital.Name[ordered_data_by_state[length(ordered_data_by_state)]]
                }
                else if(is.numeric(num)){
                        data_by_state$Hospital.Name[ordered_data_by_state[num]]
                }
                else{
                        stop("Invalid num.")
                }
        }
        
        requested_hospitals <- lapply(subset_data_by_state, get_hospital_of_rank, num)
        
        ## print(str(requested_hospitals))
        
        data.frame(hospital = unlist(requested_hospitals), state = names(requested_hospitals), row.names = names(requested_hospitals))
        
}