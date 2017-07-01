corr <- function(directory, threshold = 0) {
    
    #source("complete.R")
    comp_df <- complete(directory)
    valid_df <- comp_df[comp_df$nobs > threshold, ]
    id_list <- valid_df$id
    
    cor_list <- c()
    
    for (id in id_list) {
        if (id < 10) {
            id <- paste("00", id, sep = "")
        }
        if (id >= 10 & id < 100) {
            id <- paste("0", id, sep = "")
        }
        temp_df <- read.csv(paste(directory, "/", id, ".csv", sep = ""))
        temp_cor <- cor(temp_df$sulfate, temp_df$nitrate, use = "complete.obs")
        cor_list <- c(cor_list, temp_cor)
    }
    cor_list
}