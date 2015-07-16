pollutantmean <- function(directory, pollutant, id = 1:332)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    # Load the data from all files in the specified directory
    # HACK: This makes the assumption that separate CSV files
    # are present for each monitor ID value (e.g. 001.csv, 002.csv)
    files <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    data <- do.call("rbind", lapply(files[id], read.csv))

    mean(data[[pollutant]], na.rm = TRUE)
}