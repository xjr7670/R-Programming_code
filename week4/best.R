best <- function(st, oc) {
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome[, 11] <- as.numeric(outcome[, 11])
    outcome[, 17] <- as.numeric(outcome[, 17])
    outcome[, 23] <- as.numeric(outcome[, 23])
    
    if (is.element(st, outcome[, 7])) {
        outcome2 <- outcome[outcome[, 7] == st, c(2, 11, 17, 23)]
    } else {
        print("invalid state")
        stop()
    }
        
    
    ocs <- c("heart attack", "heart failure", "pneumonia")
    if (is.element(oc, ocs)) {
        for (i in 1:3) {
            if (oc == ocs[i]) {
                Hospital.Name <- outcome2[outcome2[, i + 1] == min(outcome2[, i + 1], na.rm = TRUE) & !is.na(outcome2[, i + 1]), 1]
                if (length(c1) > 1) {
                    Hospital.Name <- sort(Hospital.Name)[1]
                }
            }
        }
    } else {
        print("invalid outcome")
        stop()
    }
    
    Hospital.Name
    
}