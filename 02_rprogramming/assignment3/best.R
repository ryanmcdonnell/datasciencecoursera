best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate

    # Validate the outcome argument
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }

    # Load outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Reduce data to just the columns necessary and rename columns
    data <- data[c(2, 7, 11, 17, 23)]
    names(data)[1] <- "name"
    names(data)[2] <- "state"
    names(data)[3] <- "heart attack"
    names(data)[4] <- "heart failure"
    names(data)[5] <- "pneumonia"

    # Validate the state argument
    states <- unique(data$state)
    if(!(state %in% states)) {
        stop("invalid state")
    }

    # Narrow down data to state and outcome
    stateData <- data[data$state == state & data[outcome] != 'Not Available', ]

    # Coerce outcome data to numeric
    stateData[, outcome] <- as.numeric(stateData[, outcome])

    lowest <- min(stateData[, outcome])
    hospitals <- stateData[stateData[outcome] == lowest, 1]
    sorted <- sort(hospitals)
    sorted[1]
}