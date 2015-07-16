corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    result <- numeric()
    files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    for(file in files)
    {
        csv <- read.csv(file)
        if(sum(complete.cases(csv)) >= threshold)
        {
            cor <- cor(csv$nitrate, csv$sulfate, use = "na.or.complete")
            if(!is.na(cor))
                result <- append(result, cor)
        }
    }
    result
}