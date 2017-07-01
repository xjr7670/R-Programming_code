pollutantmean <- function(directory, pollutant, id = 1:332) {
    p_sum <- 0
    len <- 0
    # Read data
    for ( n in id ) {
        if ( n < 10 ) {
            n <- paste("00", n,  sep = "")
        }
        if ( n < 100 & n >= 10) {
            n <- paste("0", n, sep = "")
        }
        df1 <- read.csv(paste(directory, "/", n, ".csv", sep = ""))
        p_sum <- p_sum + sum(df1[, pollutant], na.rm = TRUE)
        len <- len + length(df1[!is.na(df1[,pollutant]), pollutant])
    }
    p_sum / len
}