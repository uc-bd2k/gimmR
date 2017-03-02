############################################################
## Name    : runPosthoc()
## Purpose : This function prepares the parameters for 
##					 running posthoc and executes "posthoc". This
##           function is also resposible for generating
##           the required outputs.
## Author  : Vinayak Kumar
############################################################

## "runPosthoc" <-
## function(commonParamsNData, matrixOut="n", readingRaw="y", probsOut="y",
##          clusterNumber=2, clusterOption="average", estimate_contexts="n") {

## above section was replaced by the below by vkj on Thu Feb 18 19:08:03 EST 2010
"runPosthoc" <-
function(commonParamsNData, matrixOut="n", readingRaw="y", probsOut="y",
         clusterNumber=2, clusterOption="average", estimate_contexts="n",
		 clientID=-1, host=-1, port=-1) {

    hostSystem<-Sys.info()["sysname"]
    ## Check if the essenstial parameter is provided or not
    if (missing(commonParamsNData)) {
        cat ("Error in runOnlyPosthoc() : Parameter commonParamsNData missing")
        return (NA)
    }

    ## Prepare the parameters file for posthoc
    ## here n_samples are equal nIter
	inputFile <- commonParamsNData$inputFile
	T <- commonParamsNData$T
	M <- commonParamsNData$M
	nChip<-commonParamsNData$nChip
	nSamples <- commonParamsNData$nSamples
	nreplicates <- commonParamsNData$nreplicates
	burnIn <- commonParamsNData$burnIn


    # set the "pparams" List as NULL
    pparams <- NULL

    # Condition for readingY
    if (nreplicates == 1) {
        readingY <- "n"
    }
    else {
        readingY <- "y"
    }

    ## Get the filname of datafile without any extension
    inputFileName <- getOnlyFileName(inputFile)

    pparams[1]<-"matrix_out"
    pparams[2] <- matrixOut
    pparams[3]<-"probs_out"
    pparams[4] <- probsOut
    pparams[5]<-"T"
    pparams[6] <- T
    pparams[7]<-"M"
    pparams[8] <- M
    pparams[9]<-"reading_raw"
    pparams[10] <- readingRaw
    pparams[11]<-"reading_y"
    pparams[12] <- readingY
    pparams[13]<-"n_samples"
    pparams[14] <- nSamples
    pparams[15]<-"burn_in"
    pparams[16] <- burnIn
    pparams[17]<-"input_file"
    pparams[18] <- inputFileName
    pparams[19]<-"cluster_number"
    pparams[20] <- clusterNumber
    pparams[21]<-"cluster_option"
    pparams[22] <- clusterOption
	if(nChip > 0) {
		pparams[23] <-"consensusType"
		pparams[24] <-.Paras$consensusType
		pparams[25] <-"nChip"
		pparams[26] <-nChip
		pparams[27] <- "EOF"
	} else {
		pparams[23] <- "EOF"
	}

    # Write the parameter.prm file once again
    if (hostSystem!="Windows") cat(pparams,file="./posthoc_parameters.prm",sep="\n")
    if (hostSystem=="Windows") cat(pparams,file="posthoc_parameters.prm",sep="\n")

    ## Calling the executables
    cat("Running posthoc Executable ..... \n")
    fileName <- paste(.PosthocPath, "posthoc", sep="")

	if(missing(clientID) & missing(host) & missing(port)) {
        fileWithOption <- paste(fileName, " -c", sep="")
	} else {
	    fileWithOption <- paste(fileName, " -c", " -i ", clientID, " -h ", host, " -p ", port, sep="")
	}

    # Call the executable
    system(fileWithOption, intern=(!(commonParamsNData$verbose)))
    
	if(estimate_contexts == "y") {
		if(missing(clientID) & missing(host) & missing(port)) {
			fileWithOption <- paste(fileName, " -C", sep = "")
		} else {
        	fileWithOption <- paste(fileName, " -C", " -i ", clientID, " -h ", host, " -p ", port, sep = "")
		}
		system(fileWithOption, intern = (!(commonParamsNData$verbose)))
	}

	## Get the data file name in correct form without the
    ## extension and location
    dataFile <- getOnlyFileName(commonParamsNData$inputFile)
    
	## Call the import Gtr method to get the clustering object from gtr file
	gtrFile <- paste("./", dataFile, ".gtr", sep="") 
    hGClustData <- importGtr(gtrFile)	

	if(estimate_contexts == "y") {
		atrFile <- paste("./", dataFile, ".atr", sep = "")
		hSClustData <- importAtr(atrFile)
	}
    ## Call the CDT file Correcting method
	if(commonParamsNData$nreplicates > 1) {
		if(estimate_contexts == "y") {
    	    cdtDataTable <- correctCdt(commonParamsNData$geneDataTable, dataFile, commonParamsNData$nreplicates, atr=hSClustData)
			clustData<-data.frame(cdtDataTable[-(1:3),-c(1,4)])
	    	geneNamesOnly<-cdtDataTable[-(1:3),c(2,3)]
		} else {
        	cdtDataTable <- correctCdt(commonParamsNData$geneDataTable, dataFile, commonParamsNData$nreplicates)
			clustData<-data.frame(cdtDataTable[-(1:2),-c(1,4)])
    		geneNamesOnly<-cdtDataTable[-(1:2),c(2,3)]
		}
		dimnames(clustData)[[2]]<-cdtDataTable[1,-c(1,4)]
		dataOnly<-data.frame(apply(clustData[,-(1:2)],2,as.double))
    	dataOnly<-dataOnly[order(hGClustData$order),]
		if(estimate_contexts == "y") dataOnly<-dataOnly[, order(hSClustData$order)]
    	geneNamesOnly<-geneNamesOnly[order(hGClustData$order),]
	    clustData[,1:2]<-geneNamesOnly
    	clustData[,-(1:2)]<-data.matrix(dataOnly)
		if(estimate_contexts == "y") colnames(clustData)[-(1:2)] <- colnames(dataOnly)
	} else {
		clustData <- commonParamsNData$geneDataTable
		if(estimate_contexts != "y") { ## need dummy sample clustering
			 hSClustData <- hclust(dist(1:(ncol(clustData)-2)))
			 hSClustData$order<-hSClustData$order[order(hSClustData$order)]
		}
		exportCdt(hr=hGClustData, hc=hSClustData, data=clustData, file=paste("./", getOnlyFileName(dataFile), ".cdt", sep=""))
	}
	if(estimate_contexts == "y") {
		resultData <- list(clustData=clustData, hGClustData=hGClustData, hSClustData=hSClustData)
	} else {
		resultData <- list(clustData=clustData, hGClustData=hGClustData)
	}
    
    ## Call the cleanFolder Function - Removes all the intermediate files
    if (commonParamsNData$intFiles == FALSE) {
		
		## Get the filname of datafile without any extension
		dataFile <- getOnlyFileName(dataFile)

		if (hostSystem!="Windows"){
			if (file.exists("posthoc_parameters.prm")) {
				cat(pparams,file="./posthoc_parameters.prm",sep="\n")
				system( "rm posthoc_parameters.prm", intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists("parameters.prm")) {
				system( "rm parameters.prm", intern=(!(commonParamsNData$verbose)) )
			}
			if ( file.exists(paste(dataFile, ".chk", sep="")) ) {
				system( paste("rm ", dataFile, ".chk", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".clusters", sep=""))) {
				system( paste("rm ", dataFile, ".clusters", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".cms", sep=""))) {
				system( paste("rm ", dataFile, ".cms", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".des", sep=""))) {
				system( paste("rm ", dataFile, ".des", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".inp", sep=""))) {
				system( paste("rm ", dataFile, ".inp", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".mpm", sep=""))) { 
				system( paste("rm ", dataFile, ".mpm", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".out", sep=""))) {
				system( paste("rm ", dataFile, ".out", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			##if (file.exists(paste(dataFile, ".pos", sep=""))) {
			##    system( paste("rm ", dataFile, ".pos", sep=""), 
			##							intern=(!(commonParamsNData$verbose)) )
			##}
			##if (file.exists(paste(dataFile, ".wc", sep=""))) {
			##	system( paste("rm ", dataFile, ".wc", sep=""), intern=(!(commonParamsNData$verbose)) )
			##}
			if (file.exists(paste(dataFile, ".x", sep=""))) {
				system( paste("rm ", dataFile, ".x", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".y", sep=""))) {
				system( paste("rm ", dataFile, ".y", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".chp", sep=""))) {
				system( paste("rm ", dataFile, ".chp", sep=""), intern=(!(commonParamsNData$verbose)) )
			}
			if (file.exists(paste(dataFile, ".cc", sep = ""))) {
				system(paste("rm ", dataFile, ".cc", sep = ""), intern = (!(commonParamsNData$verbose)))
			}
			if (file.exists(paste(dataFile, ".contexts", sep = ""))) {
				system(paste("rm ", dataFile, ".contexts", sep = ""), intern = (!(commonParamsNData$verbose)))
			}
			if (file.exists(paste("annealing", ".out", sep = ""))) {
				system(paste("rm ", "annealing", ".out", sep = ""), intern = (!(commonParamsNData$verbose)))
			}
		}
        if (hostSystem=="Windows"){
			if (file.exists("posthoc_parameters.prm")) {
		        cat(pparams,file="posthoc_parameters.prm",sep="\n")
				shell( "del posthoc_parameters.prm")
			}
			if (file.exists("parameters.prm")) {
				shell( "del parameters.prm")
			}
			if ( file.exists(paste(dataFile, ".chk", sep="")) ) {
				shell( paste("del ", dataFile, ".chk", sep=""))
			}
			if (file.exists(paste(dataFile, ".clusters", sep=""))) {
				shell( paste("del ", dataFile, ".clusters", sep=""))
			}
			if (file.exists(paste(dataFile, ".cms", sep=""))) {
				shell( paste("del ", dataFile, ".cms", sep="") )
			}
			if (file.exists(paste(dataFile, ".des", sep=""))) {
				shell( paste("del ", dataFile, ".des", sep=""))
			}
			if (file.exists(paste(dataFile, ".inp", sep=""))) {
				shell( paste("del ", dataFile, ".inp", sep="") )
			}
			if (file.exists(paste(dataFile, ".mpm", sep=""))) {
				shell( paste("del ", dataFile, ".mpm", sep="") )
			}
			if (file.exists(paste(dataFile, ".out", sep=""))) {
				shell( paste("del ", dataFile, ".out", sep="") )
			}
			##if (file.exists(paste(dataFile, ".pos", sep=""))) {
			##    shell( paste("del ", dataFile, ".pos", sep=""), 
			##							intern=(!(commonParamsNData$verbose)) )
			##}
			##if (file.exists(paste(dataFile, ".wc", sep=""))) {
			##		shell( paste("del ", dataFile, ".wc", sep="") )
			##}
			if (file.exists(paste(dataFile, ".x", sep=""))) {
				shell( paste("del ", dataFile, ".x", sep=""))
			}
			if (file.exists(paste(dataFile, ".y", sep=""))) {
				shell( paste("del ", dataFile, ".y", sep="") )
			}
			if (file.exists(paste(dataFile, ".chp", sep=""))) {
				shell( paste("del ", dataFile, ".chp", sep="") )
			}
			if (file.exists(paste(dataFile, ".contexts", sep = ""))) {
				shell(paste("del ", dataFile, ".contexts", sep = ""))
			}
			if (file.exists(paste(dataFile, ".cc", sep = ""))) {
				shell(paste("del ", dataFile, ".cc", sep = ""))
			}
			if (file.exists(paste("annealing", ".out", sep = ""))) {
				shell(paste("del ", "annealing", ".out", sep = ""))
			}
		}
    }
    return(resultData)
}
