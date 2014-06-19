## Provides the hospital with the lowest number of mortalities
## for a given outcome in a given state

# state is the 2-character abbrevation for the state

# outcome can be either "heart attack", "heart failure", or "pneumonia"

best <- function(state = NULL, outcome = "NULL") {
    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    data <- data[, c(2, 7, 11, 17, 23)]
    names(data)[3:5] <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
    data[, 3:5] <- lapply(data[, 3:5], as.numeric)
    data[, 2] <- as.factor(data[, 2])

    ## Check that state and outcome are valid
    if(is.null(state) | sum(data[2] == state) == 0)  {
        stop("invalid state")
    }

    if(tolower(outcome) == "heart attack") {
        outcome_num <- 3
    } else if(tolower(outcome) == "heart failure") {
        outcome_num <- 4
    } else if(tolower(outcome) == "pneumonia") {
        outcome_num <- 5
    } else {
        stop("invalid outcome")
    }

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data <- data[data$State == state, ]
    data <- data[complete.cases(data$Hospital.Name, data[[outcome_num]]), ]
    hospital_name <- 
        data[data[outcome_num] == min(data[outcome_num], na.rm = TRUE), ]$Hospital.Name
    hospital_name
}