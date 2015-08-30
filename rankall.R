## The function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the 
## ranking specified in num for the given outcome.

rankall <- function(outcome, num = "best") {
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
    ##if (!(state %in% data.subset1$State)) {
    ##    stop("invalid state")
    ##}
    if (!(outcome %in% names(data.subset1.idx))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    split.by.state <- split(data.subset1, data.subset1$State)
    result <- data.frame()
    for (idx in 1:length(split.by.state)) {
        temp.data <- as.data.frame(split.by.state[idx])
        ordered.data <- temp.data[order(temp.data[, data.subset1.idx[outcome]],
                                        temp.data[, 1], na.last = NA), ]
        if (identical(num, "best")) {
            item <- data.frame(hospital = ordered.data[1, 1], 
                               state = ordered.data[1, 2])
        }
        else if (identical(num, "worst")) {
            last.row <- nrow(ordered.data)
            item <- data.frame(hospital = ordered.data[last.row, 1], 
                               state = ordered.data[last.row, 2])
        }
        else {
            item <- data.frame(hospital = ordered.data[num, 1], 
                               state = ordered.data[num, 2])
        }
        result <- rbind(result, item)
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result
}