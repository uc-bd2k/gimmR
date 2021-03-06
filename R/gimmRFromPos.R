############################################################
## Name    : gimmRFromPos()
## Purpose : After finishsing all the processes related to
##           Gimm and Posthoc,.pos file is generated which
##           contains smoother data. This function gets the
##           data from this file and use this data to find
##           new gimmR Data which can be used to see the 
##           heatmap of the genes.

## N.B     : Since .pos file contains the exactly T number
##           of rows, so to process thsi data, the nreplicates
##           will be always considered as 1. All other 
##           parameters will be same as earlier.
##
## Author  : Vinayak Kumar
############################################################
gimmRFromPos <-
function(posDataFileName, M, T, nIter=10000, nreplicates=1,
				 contextSpecific="n", nContexts=1, contextLengths=NA,
				 clusterShape="v", burnIn=5000, elipticalWithin="n", matrixOut="n",
				 readingRaw="y", probsOut="y", clusterNumber=2,
				 clusterOption="average", verbose=FALSE, intFiles=FALSE) {

		hostSystem<-Sys.info()["sysname"]
		if (is.na(.GeneDataTable[1,1])) {
			cat("Please run gimm and posthoc first, then use this method")
		}
		else {
				if (missing(posDataFileName)) {
						posDataFileName <- "Result"
				}
				## get the table from .pos file. This table will utilize the genenames
				## and ORF information from the .GeneDataTable
				posDataTable <- NA
				
				## Get the name of file without any extension
				posDataFileName <- getOnlyFileName(posDataFileName)
				if (hostSystem!="Windows") posDataFileName <- paste("./", posDataFileName, ".pos", sep="")

				## if file doesn't exist in the current folder
				if (!file.exists(posDataFileName)) {
						cat(posDataFileName, " doesn't exists in the current folder!", 
								".. Exiting genPosTable() function \n")
				}
				## means that file is in current folder
				else {
						## if the file exists	
						## First line is garbled, so skipped
						posDataTable <- read.table(posDataFileName, skip=1)

						##Get the number of rows		
						noOfRowsInPos <- nrow(posDataTable)
						noOfColsInPos <- ncol(posDataTable)

						## Read the names provided by the .GeneDataTable
						geneDataTableColNames <- colnames(.GeneDataTable)
						noOfRowsInTxtData <- nrow(.GeneDataTable)
						noOfColsInTxtData <- ncol(.GeneDataTable)

						## Check if the nreplicates parameter is missing
						if (missing(nreplicates)) {
							nreplicates <- noOfRowsInTxtData / noOfRowsInPos
						}

						## Create a Matrix which will contain all the data
						## Total no of Rows will be equal to the no of rows
						## in PosDataTable while no of cols will be same
						## as the no of cols in .GeneDataTable
						posDataMat <- matrix(0, nrow=noOfRowsInPos,
																 ncol=noOfColsInTxtData)
						
						## Now get the data from posDataTable to matrix
						posDataMat[, 3:noOfColsInTxtData] <- as.matrix(
																		posDataTable[, 2:noOfColsInPos])
						
						## get the sequence for rows
						rowsSeq <- c(1:noOfRowsInPos)
						for (i in rowsSeq) {
							## for the genenames
							posDataMat[i, 1] <- .GeneDataTable[(i * nreplicates),1]

							## for the ORFs
							posDataMat[i, 2] <- .GeneDataTable[(i * nreplicates),2]
						}
						
						if (hostSystem!="Windows") write.table(posDataMat[,], "./temp.tmp", sep="\t", dec=".",
										row.names=FALSE, col.names=FALSE, quote=FALSE)
						if (hostSystem=="Windows") write.table(posDataMat[,], "temp.tmp", sep="\t", dec=".",
										row.names=FALSE, col.names=FALSE, quote=FALSE)

						## Convert to the table object
						## Read the table
						if (hostSystem!="Windows") posDataTable <- read.table("./temp.tmp")
						if (hostSystem=="Windows") posDataTable <- read.table("temp.tmp")

						## Remove the file
						if (hostSystem!="Windows") system ("rm ./temp.tmp")
						if (hostSystem=="Windows") system ("rm temp.tmp")

						## set the col names
						colnames(posDataTable) <- geneDataTableColNames
				}


				## Call the runGimmNPosthoc on this object with nreplicates as 1
				posGimmR <- runGimmNPosthoc(posDataTable, posDataFileName, M, T,
																nIter, nreplicates=1, contextSpecific,
																nContexts, contextLengths, clusterShape,
                                burnIn, elipticalWithin, matrixOut,readingRaw,
                                probsOut, clusterNumber, clusterOption,
                                verbose, intFiles)

				return(posGimmR)
		}
}
