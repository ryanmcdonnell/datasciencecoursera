rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

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

    # Get list of unique states
    states <- unique(data$state)
    # Order the states
    states <- states[order(states)]

    allrank <- data.frame(matrix(nrow = length(states), ncol = 2), row.names = states)
    colnames(allrank) <- c("hospital", "state")

    for(state in states) {
        allrank[state, 2] <- state

        # Narrow down data to state and outcome
        stateData <- data[data$state == state & !is.na(data[outcome]), ]
        numOfHospitals <- nrow(stateData)

        switch(num, best = { num <- 1 }, worst = { num <- numOfHospitals})
        if(num > numOfHospitals) {
            allrank[state, 1] <- NA
        }

        # Coerce outcome data to numeric
        stateData[, outcome] <- as.numeric(stateData[, outcome])

        # Order data by outcome and then hospital name
        sortOrder <- order(stateData[, outcome], stateData[, 1])
        ranked <- stateData[sortOrder, ]

        allrank[state, 1] <- ranked[num, 1]
    }

    allrank
}