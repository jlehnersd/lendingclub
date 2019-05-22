# Input:  Character array of csv data file names from LendingClub.
#
# Output: Data frame created by binding the rows of the individual data frames
#         read in from the csv files.

load_csvdata <- function(filenames) {
    
    nFiles <- length(filenames)
    data_list <- list()
    
    for(iFile in 1:nFiles) {
        file_ <- paste("data/", filenames[iFile], sep = "")
        data_list[[iFile]] <- read.csv(file = file_, skip = 1)
    }
    
    csvdata <- bind_rows(data_list)
    return(csvdata)
}
