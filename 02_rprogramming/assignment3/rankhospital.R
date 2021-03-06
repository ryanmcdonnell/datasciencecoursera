rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    # Validate the outcome argument
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }

    # Load outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
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
    stateData <- data[data$state == state & !is.na(data[outcome]), ]
    numOfHospitals <- nrow(stateData)

    switch(num, best = { num <- 1 }, worst = { num <- numOfHospitals})
    if(num > numOfHospitals) {
        return(NA)
    }

    # Coerce outcome data to numeric
    stateData[, outcome] <- as.numeric(stateData[, outcome])

    # Order data by outcome and then hospital name
    sortOrder <- order(stateData[, outcome], stateData[, 1])
    ranked <- stateData[sortOrder, ]

    ranked[num, 1]
}

# Sample runs:
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"

# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"

# > rankhospital("MN", "heart attack", 5000)
# [1] NA