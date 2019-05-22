# Input:  1) Character array of LendingClub URLs to download raw csv data.
#         2) Character array of default csv filenames from LendingClub website.
#
# Output: None (downloads and extracts LendingClub data).

get_rawdata <- function(urls, filenames) {
    
    # ADD ERROR CHECK FOR urls AND filenames BEING SAME LENGTH
    
    nFiles <- length(filenames)
    rawdata <- list()
    
    for(iFile in 1:nFiles) {
        
        # Get URL and filename pair
        url <- urls[iFile]
        file <- paste("data/", filenames[iFile], sep = "")
        zipfile <- paste(file, ".zip", sep = "")
        
        # Download and extract files only if absent from data directory
        if(!file.exists(file)) {
            if(!file.exists(zipfile)) {
                download.file(url, destfile = zipfile) # Macs: method = "curl"
            }
            unzip(zipfile, exdir = "data")
        }
    }
    
    return()
}