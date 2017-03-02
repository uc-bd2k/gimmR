############################################################
## Name    : readMpm()
## Purpose : This function reads the .mpm file and returns
##					 it as table object
## Author  : Vinayak Kumar
############################################################
"readMpm" <-
function (dataFile) {

		hostSystem<-Sys.info()["sysname"]
		mpmDataTable <- NA
		if (missing(dataFile)) {
				if (hostSystem!="Windows") dataFile <- "./Result.mpm"
				if (hostSystem=="Windows") dataFile <- "Result.mpm"
		}
		else {
				dataFile <- getOnlyFileName(dataFile)
				if (hostSystem!="Windows") dataFile <- paste("./", dataFile, ".mpm", sep="")
				if (hostSystem=="Windows") dataFile <- paste(dataFile, ".mpm", sep="")
		}

		if (!file.exists(dataFile)) {
				cat(dataFile, " doesn't exists in the current folder!", 
						".. Exiting readMpm() function \n")
			 return (mpmDataTable)
		}
		else {
				## Read the file
				y <- readLines(dataFile)

				## getthe index of curr_iter
				curr_iterInd <- grep("curr_iter", y)

				## get the index of "Time used"
				## Get the index of second value of Time used
				timeUsedInd <- grep("Time used", y)[2]

				## Get  the index of the line before thi time used which is non-empty
				loopVal <- c( (timeUsedInd[1] - 1 ): curr_iterInd)

				lastInd <- curr_iterInd

				for (i in loopVal) {
					if (y[i] != "") {
						lastInd = i
						break
					}
				}

				if (lastInd == curr_iterInd) {
					cat (" Last Index is not found")
				}

				## Get he no of the rows in the matrix
				noOfRows <- lastInd - curr_iterInd + 1


				## get the no of the colums by spliting the line containing curr_iter
				noOfCols <- length(strsplit(y[curr_iterInd], "\t") [[1]])

				##Now split all the lines an put them in the matrix create.
				mpmMat <- matrix(nrow = noOfRows, ncol=noOfCols)

				loopVal <- c(curr_iterInd : lastInd)

				colLoopVal <- c (1:noOfCols)

				for (i in loopVal) {
					temp <- strsplit(y[i], "\t")

					for (j in colLoopVal) {
						mpmMat[(i - curr_iterInd + 1), j] <- temp[[1]][j]
					}
				}
				## write this matrix in a temporary file an rea it back as table

				write.table(mpmMat, "mpmTemp.tmp", sep="\t", dec=".",
										row.names=FALSE, col.names=FALSE, quote=FALSE)

				if (hostSystem!="Windows") mpmDataTable <- read.table("./mpmTemp.tmp", header=TRUE)
				if (hostSystem=="Windows") mpmDataTable <- read.table("mpmTemp.tmp", header=TRUE)

				## now delete the file
				if (hostSystem!="Windows") system("rm ./mpmTemp.tmp", intern=TRUE)
				if (hostSystem=="Windows") shell("del mpmTemp.tmp")

				return (mpmDataTable)
		}
}
