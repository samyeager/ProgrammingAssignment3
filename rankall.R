## Provides the hospital with a certain rank for a given outcome for each state.

# outcome can be either "heart attack", "heart failure", or "pneumonia"

# num is the rank (1 = "best") that corresponds to outcome. integers, "best", 
# and "worst" are allowed

rankall <- function(outcome = "NULL", num = "best") {

    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    data <- data[, c(2, 7, 11, 17, 23)]
    names(data)[3:5] <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
    data[, 3:5] <- lapply(data[, 3:5], as.numeric)
    
    ## Check that outcome is valid
    if(tolower(outcome) == "heart attack") {
        outcome_num <- 3
    } else if(tolower(outcome) == "heart failure") {
        outcome_num <- 4
    } else if(tolower(outcome) == "pneumonia") {
        outcome_num <- 5
    } else {
        stop("invalid outcome")
    }
    
    ## Process the data so that it is complete, ordered, and factored by state.
    data <- data[complete.cases(data$Hospital.Name, data[[outcome_num]]), ]
    data <- data[order(data$State, data[[outcome_num]], data$Hospital.Name), ]
    data_st <- split(data, data$State)

    ## Check that num is valid
    if(num == "best") {
        rank <- rep(1, length(data_st))
    } else if(num == "worst") {
        rank <- unsplit(lapply(data_st, nrow), names(data_st))
    } else if(!is.na(as.integer(num))) {
        rank <- rep(as.integer(num), length(data_st))
    }
    names(rank) <- names(data_st)
    
    ## For each state, find the hospital of the given rank
    hospital_entry <- NULL
    for(i in names(data_st)) {
        hospital_entry <- rbind(hospital_entry, data_st[[i]][rank[i], 1])
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hospital_rank <- data.frame(hospital = as.character(hospital_entry), 
                                state = names(data_st), 
                                row.names = names(data_st))    
    hospital_rank

}