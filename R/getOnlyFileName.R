############################################################
## Name    : getOnlyfileName()
## Purpose : This function retrives the filename from the 
##					 string provided wituhout any extension and 
##					 path information
## Author  : Vinayak Kumar
############################################################

"getOnlyFileName" <-
function(filePath) {
	
    ## Get the number of char in the filePath
    noOfChar <- nchar(filePath)
	
    i <- noOfChar
    fileNameStart <- 0
    fileNameEnd <- noOfChar

    ## Loop untill the "/" character is reached or the start of string 
    while ( (substr(filePath, i, i) != "/") && (i != 0) ) {
        if (substr(filePath, i, i) == ".") {
            ## File name will end before extension starts
            fileNameEnd <- i - 1
        }
        i <- i - 1
    }
	
    fileNameStart <- i + 1

    ## Return the file name
    return (substr(filePath, fileNameStart, fileNameEnd))
}
#Zhen
"getPriorType" <-
function(filename) 
{
	isCategoryPrior <- 'y'
	data<-read.table(filename,header=TRUE)
	if(dim(data)[2] == 3) isCategoryPrior <- 'y'
	else
	{
		if(dim(data)[2] == dim(data)[1] + 2) isCategoryPrior <- 'n'
		else	cat("Wrong prior data type\n")
	}
	rm(data)
	return(isCategoryPrior)
}