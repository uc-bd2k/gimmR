############################################################
## Name    : deleteFiles()
## Purpose : This function deletes the intermediate files
##           as asked by the user
## Author  : Vinayak Kumar
############################################################
"deleteFiles" <-
function(dataFile, zm=FALSE, pos=FALSE, mpm=FALSE, verbose=FALSE) {

		hostSystem<-Sys.info()["sysname"]
		if (missing(dataFile)) {
				dataFile <- "Result"
		}
		else {
				## Get the filname of datafile without any extension
				dataFile <- getOnlyFileName(dataFile)
		}
		
		if (mpm) {
				if (file.exists(paste(dataFile, ".mpm", sep=""))) {
				    if (hostSystem=="Windows") shell(paste("del ", dataFile, ".mpm", sep=""))
				    else{
					system( paste("rm ", dataFile, ".mpm", sep=""),
						intern=(!(verbose)) )
					cat (paste(dataFile, ".mpm", sep=""), " is deleted. \n")
				   }
				}
				else {
					cat(paste(dataFile, ".mpm", sep=""), " doesn't exist. \n")
				}
		}

		if (zm) {
				if (file.exists(paste(dataFile, ".zm", sep=""))) {
				    if (hostSystem=="Windows") shell(paste("del ", dataFile, ".zm", sep=""))
				    else{
					system( paste("rm ", dataFile, ".zm", sep=""),
						intern=(!(verbose)) )
					cat (paste(dataFile, ".zm", sep=""), " is deleted. \n")
				    }
				}
				else {
					   cat(paste(dataFile, ".zm", sep=""), " doesn't exist. \n")
				}
		}

 		if (pos) {
				if (file.exists(paste(dataFile, ".pos", sep=""))) {
				    if (hostSystem=="Windows") shell(paste("del ", dataFile, ".pos", sep=""))
				    else{
					system( paste("rm ", dataFile, ".pos", sep=""),
					       intern=(!(verbose)) )
					cat (paste(dataFile, ".pos", sep=""), " is deleted. \n")
				   }
				}
				else {
					  cat(paste(dataFile, ".pos", sep=""), " doesn't exist. \n")
				}
		}
 }

