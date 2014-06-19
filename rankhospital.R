## Provides the hospital name at a certain rank and stae based on the outcome.

# state is the 2-character abbrevation for the state

# outcome can be either "heart attack", "heart failure", or "pneumonia"

# num is the rank (1 = "best") that corresponds to outcome. integers, "best", 
# and "worst" are allowed

rankhospital <- function(state = NULL, outcome = "NULL", num = "best") {

    ## Use a helper function that does most of the work already
    source("rankall.R")
    hospital_rank <- rankall(outcome, num)
    hospital_rank[, 1] <- as.character(hospital_rank[, 1])
    
    ## Check that state and outcome are valid
    if(is.null(state) | sum(hospital_rank[, 2] == state) == 0)  {
        stop("invalid state")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    hospital_rank[hospital_rank$state == state, 1]

}