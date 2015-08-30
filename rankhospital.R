## This function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking specified 
## by the num argument for the given outcome in that state.

rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data.subset2 <- subset(data.subset1, data.subset1$State == state &
                               !is.na(data.subset1[[data.subset1.idx[outcome]]]),
                           select = c(Hospital.Name, data.subset1.idx[outcome]))
    final.subset <- data.subset2[order(data.subset2[, 2], data.subset2[, 1]), ]
    if (identical(num, "best")) {
        final.subset[1, 1]
    }
    else if (identical(num, "worst")) {
        final.subset[nrow(final.subset), 1]
    }
    else {
        final.subset[num, 1]
    }
}