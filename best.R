## This function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best (i.e. lowest) 
## 30-day mortality for the specified outcome in that state.

best <- function(state, outcome) {
    ## Read outcome data
    data.file <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",
                          colClasses = "character")
    data.file[, 11] <- as.numeric(data.file[, 11])
    data.file[, 17] <- as.numeric(data.file[, 17])
    data.file[, 23] <- as.numeric(data.file[, 23])
    
    ## Get data subset of hospital name, state, and death rates from different
    ## outcomes
    data.subset1 <- data.file[, c(2, 7, 11, 17, 23)]
    data.subset1.idx <- c("heart attack" = 3, "heart failure" = 4, "pneumonia" = 5)
    
    ## Check that state and outcome are valid
    if (!(state %in% data.subset1$State)) {
        stop("invalid state")
    }
    if (!(outcome %in% names(data.subset1.idx))) {
        stop("invalid outcome")
    }
    
    ## Further subset the data by input state and minimum death rate from
    ## input outcome
    data.subset2 <- subset(data.subset1, data.subset1$State == state,
                           select = c(Hospital.Name, data.subset1.idx[outcome]))
    final.subset <- data.subset2[which(data.subset2[, 2] == min(data.subset2[, 2], 
                                                          na.rm = TRUE)), 1]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (length(final.subset) > 1) {
        answer <- sort(final.subset)
        answer[1]
    }
    else {
        final.subset[1]
    }
}