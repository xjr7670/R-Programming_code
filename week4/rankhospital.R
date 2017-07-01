rankhospital <- function(st, oc, num = "best") {
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
                sub_df <- outcome2[order(outcome2[, i+1], outcome2[, 1], na.last = NA), c(1, i+1)]
                if (num == "best") {
                    Hospital.Name = sub_df[1, 1]
                } else if (num == "worst") {
                    Hospital.Name = sub_df[nrow(sub_df), 1]
                } else {
                    Hospital.Name = sub_df[num, 1]
                }
            }
        }
    } else {
        print("invalid outcome")
        stop()
    }
    
    Hospital.Name
}