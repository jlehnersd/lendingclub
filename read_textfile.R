# Input:  Character array of txt file directories.
#
# Output: List of character arrays. Each array in the list is obtained by
#         reading in a single-column txt file.

read_textfile <- function(filenames) {
    
    nFiles <- length(filenames)
    txt_list <- list()
    
    for(iFile in 1:nFiles) {
        file_ <- filenames[iFile]
        txt_list[[iFile]] <- scan(file = file_, what = character())
    }
    
    return(txt_list)
}