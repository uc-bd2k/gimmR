############################################################
## Name    : correctCdt()
## Purpose : This function corrects the .CDT file generated
##					 by posthoc. The new generated file will be 
##					 compatible for treeview
## Author  : Vinayak Kumar
############################################################
"correctCdt" <-
function(geneDataFromTxt, fileName="", nreplicates=1, atr=NULL) {

    
    ## Get the filname of datafile without any extension
    fileName <- getOnlyFileName(fileName)

    # append ./ in the fileName
    cdtFileName <- paste("./", fileName, ".cdt", sep="")
    cdtData <- NULL

    # open a connection to the file 
    con <- file(cdtFileName)

    # Read the first line
    firstLine <- scan(con, what="character", sep="\t", nlines=1)

    ## Get the column names from the Txt Gene Data table
	colNames <- colnames(geneDataFromTxt)

	# Read the second line
    secondLine <- scan(con, what="character", sep="\t", nlines=1, skip=1)

    # Read other Lines
    otherLines <- scan(con, what="character", sep="\t", skip=2)

    # close the connection
    close(con)

    # Get the number of columns in the first Line
    noOfCols <- length(firstLine)
    countCols <- c(1:noOfCols)

    # get the number of rows of he data in the CDT file
    # This excludes the first two lines
    noOfDataRowsInCdt <- length(otherLines)/noOfCols
    cdtDataTable <- matrix(nrow=(noOfDataRowsInCdt + 2), ncol=noOfCols)	
	
		
    # Put the values of the first line in the cdtDataTable
	## first line will be the header of the file, it should 
	## contain proper experiment names which can be taken
	## from colNames
    for (i in countCols) {
		if (i <= 4) {
			cdtDataTable[1, i] <- firstLine[i]
		} else {
			## 5th element will contain the name of 3rd colName
			cdtDataTable[1, i] <- colNames[i - 2]
		}
		if (i >= 2 && i <= 4) {
			cdtDataTable[2, i] <- ""
		} else {
			cdtDataTable[2, i] <- secondLine[i]
		}
    }

    countRows <- c(3:(noOfDataRowsInCdt + 2))
	
    # Put the rest of the data in the Table from OtherLines object
    for (i in countRows) {
        for (j in countCols) {
            cdtDataTable[i, j] <- otherLines[(i - 3) * noOfCols + j]
        }
    }
		
    ## Get the number of rows in the the CDT file
		## This includes the Two header rows too
    noOfRowsInCdt <- nrow(cdtDataTable)

	## Get the Numeric Values from the column named "ACC"
	## The index of acc.numeric will be from 1 to (noOfRowsInCdt - 2)
	##acc.numeric<-as.integer(substring(cdtDataTable[3:noOfRowsInCdt,2],2))
	if (nreplicates == 1) {
		acc.numeric<-as.integer(substring(cdtDataTable[3:noOfRowsInCdt,2], 2))
	}
	else {
		acc.numeric<-as.integer(cdtDataTable[3:noOfRowsInCdt,2])
	}
		
	## cat ("Original", cdtDataTable[3:noOfRowsInCdt, 2], "\n")
	## cat ("Trimmed", acc.numeric, "\n")

	## Order the acc.numneric in ascending order
	# geneIdsOrder<-order(acc.numeric)
	## Confirm it once again
	# geneIdsOrderOrder<-order(geneIdsOrder)
	

	## get a Sequence based on nreplicates
	neededGeneRowsNoFromTxt <- seq(1, ((noOfRowsInCdt - 2 ) * nreplicates), nreplicates)
	neededGeneRowsNo <- c(1 : length(neededGeneRowsNoFromTxt))

    orfData <- c(1:noOfRowsInCdt - 2)
    geneNameData <- c(1:noOfRowsInCdt - 2)

	for (i in neededGeneRowsNo) {
    # Get the first col of text data in orfData
		reqRowNum <- neededGeneRowsNoFromTxt[neededGeneRowsNo]

		## Since table is used to read these data,
		## the HEADER of the table should not be counted
		## as a ROW!!
        orfData[neededGeneRowsNo] <- as.character(geneDataFromTxt[(reqRowNum), 1])

        # Get the second col of text data in geneNameData
        geneNameData[neededGeneRowsNo] <- as.character(geneDataFromTxt[(reqRowNum), 2])
    }

	#########Previous code for data transfer from txt table########
	##cdtDataTable[3 : noOfRowsInCdt, 2] <- orfData[1 : (noOfRowsInCdt - 2)]
    ##cdtDataTable[3 :noOfRowsInCdt, 3] <- geneNameData[1 :(noOfRowsInCdt - 2)]
    ################################################################

	# There may be lesser number of Rows in Cdt than the .txt Data
    # The Data in the Second column of CDT table will be replaced by the 
    #  orfData
 	## Here the data will be entered in the order of geneIdsOrderOrder
    cdtDataTable[3 : noOfRowsInCdt, 2] <- orfData[acc.numeric]
		
    # The Data in the Third column of CDT table will be replaced by the 
    #  geneNameData
	## Here the data will be entered in the order of geneIdsOrderOrder
    cdtDataTable[3 : noOfRowsInCdt, 3] <- geneNameData[acc.numeric]

	if(!is.null(atr)) { ##if context estimation was run
		cdtDataTable[, -(1:4)] <- (cdtDataTable[, -(1:4)])[, atr$order]
		cdtDataTable <- rbind(cdtDataTable[1,], c("AID", NA, NA, NA, paste("ARRY", atr$order, "X", sep = "")), cdtDataTable[-1,])
	}
    write.table(cdtDataTable[,], cdtFileName, sep="\t", dec=".", row.names=FALSE, col.names=FALSE, quote=FALSE)
    return(cdtDataTable)
}

