rank_hospital_by_state <- function(li, num2, rank_num) {
    data_df <- as.data.frame(li)
    
    temp_df <- data_df[order(data_df[, num2], data_df[, 1], na.last = NA), ]
    
    if (rank_num == "best") {
        row_num = 1
    } else if (rank_num == "worst") {
        row_num = nrow(temp_df)
    } else {
        row_num = rank_num
    }
    
    result_df <- temp_df[row_num, 1]
    result_df
}

rankall <- function(oc, num = "best") {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome[, 11] <- as.numeric(outcome[, 11])
    outcome[, 17] <- as.numeric(outcome[, 17])
    outcome[, 23] <- as.numeric(outcome[, 23])
    
    outcome2 <- outcome[, c(2, 7, 11, 17, 23)]
    ocs <- c("heart attack", "heart failure", "pneumonia")
    if (is.element(oc, ocs)) {
        outcome_list <- split(outcome2, outcome2[, 2])
        if (oc == "heart attack") {
            col_num = 3
        } else if (oc == "heart failure") {
            col_num = 4
        } else {
            col_num = 5
        }
        result_list <- lapply(outcome_list, rank_hospital_by_state, num2 = col_num, rank_num = num)
        result_matrix <- as.matrix(result_list)
        result_df <- as.data.frame(result_matrix)
        col_names <- c("hospital", "state")
        result_df$state <- row.names(result_df)
        colnames(result_df) <- col_names
    } else {
        print("invalid outcome")
        stop()
    }
    
    result_df
}