complete <- function(directory, id = 1:332) {
    
    df <- data.frame()
    
    for (n in id) {
        if (n < 10) {
            n <- paste("00", n, sep = "")
        }
        if (n < 100 & n >= 10) {
            n <- paste("0", n, sep = "")
        }
        temp_df = read.csv(paste(directory, "/", n, ".csv", sep = ""))
        not_na_sul <- !is.na(temp_df$sulfate)
        not_na_nit <- !is.na(temp_df$nitrate)
        df2 = temp_df[not_na_sul & not_na_nit, ]
        nobs_num <- dim(df2)[1]
        
        temp2_df <- data.frame(id = as.numeric(n), nobs = nobs_num)
        df <- rbind(df, temp2_df)
    }
    df
}