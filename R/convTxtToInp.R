############################################################
## Name    : convTxtToInp()
## Purpose : This function takes the genes data file
##					 (of any extenstion) as input and produces a
##					 .inp file for processing by "gimm".
## Author  : Vinayak Kumar
############################################################

"convTxtToInp" <-
function(fileName = "", M) {

    txtGeneData <- NA

    ## if file doesn't exist in the current folder
    if (!file.exists(fileName)) {
        cat(fileName, " does not exists in the current folder!", 
            ".. Exiting convTxtToInp() function \n")
        return (txtGeneData)
    }
    ## means that file is in current folder
    else {

				## if the file exists	
				## Header will be created from first line of file		
        txtGeneData <- read.table(fileName, header=TRUE)

        ##Get the number of rows		
        noOfRows <- nrow(txtGeneData)
        noOfCols <- ncol(txtGeneData)
				
				## Check whether the no of columns are M + 2 or not
				if (noOfCols != (M + 2)) {
					cat(fileName, " No. of columns in the text file should be M + 2", 
            ".. Exiting convTxtToInp() function \n")
				  txtGeneData <- NA
					## Return NA as output which will provide info to upper level
					## funciton that something has gone wrong with this function.
					return (txtGeneData)
				}
        ## Retrives the filename without any extension	
        inpFileName <- getOnlyFileName(fileName)

        ## File connection is opened for writing
        inpFileName <- paste("./", inpFileName, ".inp", sep="")

        ## Write the data
        write.table(txtGeneData[ , c(-1,-2)], inpFileName, sep="\t",
                    dec=".", row.names=FALSE, col.names=FALSE, quote=FALSE)
        ## Returns the Whole of the Table read from the txt file
        ## this object will be used for writing some data in .cdt file
#         .GeneDataTable <<- txtGeneData

				return (txtGeneData)
    }
}
#zhen
"convTxtAndPriorToInp" <-
function(fileName = "", priorFilename = "", M) {

    txtGeneData <- NA

    ## if file doesn't exist in the current folder
    if (!file.exists(fileName) | !file.exists(priorFilename)) {
        cat(fileName, " or ", priorFilename, " does not exists in the current folder!", 
            ".. Exiting convTxtToInp() function \n")
        return (txtGeneData)
    }
    ## means that file is in current folder
    else {

				## if the file exists	
				## Header will be created from first line of file		
        txtGeneData <- read.table(fileName, header=TRUE)
	txtPriorData<- read.table(priorFilename, header=TRUE)

        ##Get the number of rows		
        noOfRows <- nrow(txtGeneData)
        noOfCols <- ncol(txtGeneData)
				
				## Check whether the no of columns are M + 2 or not
				if (noOfCols != (M + 2)) {
					cat(fileName, " No. of columns in the text file should be M + 2", 
            ".. Exiting convTxtToInp() function \n")
				  txtGeneData <- NA
					## Return NA as output which will provide info to upper level
					## funciton that something has gone wrong with this function.
					return (txtGeneData)
				}
				#########Combine expression data and prior data together
				if(dim(txtPriorData)[1] != noOfRows)
				{
					combineTable<-txtGeneData[,c(-1,-2)]
					cat("Incorrect number of rows for prior information\n")
				}
				else
				{
					if(sum(txtGeneData[,1] != txtPriorData[,1]) > 0)
						cat("Expression data and prior data do not match\n")
					else
						combineTable<-cbind(txtGeneData[,c(-1,-2)],txtPriorData[,c(-1,-2)])
				}
        ## Retrives the filename without any extension	
        inpFileName <- getOnlyFileName(fileName)

        ## File connection is opened for writing
        inpFileName <- paste("./", inpFileName, ".inp", sep="")

        ## Write the data
        write.table(combineTable, inpFileName, sep="\t",
                    dec=".", row.names=FALSE, col.names=FALSE, quote=FALSE)
        ## Returns the Whole of the Table read from the txt file
        ## this object will be used for writing some data in .cdt file
#         .GeneDataTable <<- txtGeneData

				return (txtGeneData)
    }
}
