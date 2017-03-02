############################################################
## Name    : runGimm()
## Purpose : This function prepares the parameters for 
##					 running gimm and executes "gimm" only.
## Author  : Vinayak Kumar
############################################################
## "runGimm" <-
## function(tableData, dataFile, M, T, nChip=0, nIter=10000, nreplicates=1,
## 				 contextSpecific="n", nContexts=1, contextLengths=NA, 
## 				 estimate_contexts="n", clusterShape="v", burnIn=5000, 
## 				 elipticalWithin="n", verbose=FALSE, intFiles=FALSE) {

## Above section was replaced by the below by vkj on Thu Feb 18 18:09:57 EST 2010
"runGimm" <-
function(tableData, dataFile, M, T, nChip=0, nIter=10000, nreplicates=1,
				 contextSpecific="n", nContexts=1, contextLengths=NA, 
				 estimate_contexts="n", clusterShape="v", burnIn=5000, 
				 elipticalWithin="n", verbose=FALSE, intFiles=FALSE, clientID=-1, host=-1, port=-1, priorFile=NULL) {

    hostSystem<-Sys.info()["sysname"]

    ## If the tableData object is available, but
    ## dataFile is missing then the default file name 
    ## will be "Result". This file will be used to deliver
    ## the results of execution of gimm and posthoc via
    ## cdt and gtr files
    if ((!missing(tableData)) && (missing(dataFile)) ) {
        dataFile <- "Result"
    }

    ## Check if the essenstial parameters are provided or not
    if (missing(M)) {
        cat ("Error in processGimmParas() : Parameter M missing")
        return ("")
    }
    if (missing(T)) {
        cat ("Error in processGimmParas() : Parameter T missing")
        return ("")
    }
#    if (missing(nChip)) {
#	cat ("Error in processGimmParas() : Parameter nChip missing")
#	return ("")
#    }

    if (missing(dataFile)) {
        cat ("Error in processGimmParas() : Parameter dataFile missing")
        return ("")
    }

    ##if (missing(tableData)) tableData <- NA

    ## Run the prepForGimm(0 to prepare the environment
    ## of running gimm executable
    ## Get the Table ofthe Data from the Gene Data File
    geneDataTable <- NA
	
    ## If the tableData object is available, but
    ## dataFile is missing then the default file name 
    ## will be "Result". This file will be used to deliver
    ## the results of execution of gimm and posthoc via
    ## cdt and gtr files
    if ( !(missing(tableData)) && (missing(dataFile)) ) {
        dataFile <- "Result"
    }

    ## set the "params" List as NULL
    params <- NULL

	if(missing(nContexts) & estimate_contexts=="y") 
	    nContexts <- M
	if((missing(contextLengths) | is.na(contextLengths[1])) & estimate_contexts=="y") {
		nContexts <- M
	    contextLengths <- rep(1, M)
	}
#    if (missing(contextSpecific)) {
##########Xiangdong
#        if (nContexts == 1 && nChip == 0) {
#            contextSpecific <- "n"
#        }
#        else {
#            contextSpecific <- "y"
#        }
#    }
	
    #Mario
    if ((contextSpecific == "n" & estimate_contexts == "n") | nContexts == 1) {
        contextLengths <- M
    }
	


    ## Error Condition for contextSpecific and NContexts
 #   if ( (nContexts > 1 || nChip > 0) && (contextSpecific == "n") ) {
 #       cat ("Error in runGimm() : When NContexts > 1",
 #            " contextSpecific should be \"y\"")
 #       return ("")
 #   }

    if (contextSpecific == "y" | estimate_contexts=="y") {
        if(sum(contextLengths)!= M){
					cat ("Error in runGimm() : context lengths do not sum to M\n")

					print (contextLengths)
					print(sum(contextLengths))
					print(M)
					return ("")
        }
    }

    ## Get the filname of datafile without any extension
    dataFileName <- getOnlyFileName(dataFile)

    ## Print the Inforamtion reagrding the Parameters in
    ## the file and their values
    params[1] <- "Q"
    params[2] <- .Paras$Q
    params[3] <- "alpha"
    params[4] <- .Paras$alpha
    params[5] <- "average"
    params[6] <- .Paras$average
    params[7] <- "nreplicates"
    params[8] <- nreplicates
    params[9] <- "automatic_annealing"
    params[10] <- .Paras$automatic_annealing
    params[11] <- "context_specific"
    params[12] <- contextSpecific
	params[13] <- "estimate_contexts"
	params[14] <- estimate_contexts
###############################################M=M+nChip??? 
    params[15] <- "M"
    params[16] <- M+nChip
    params[17] <- "T"
    params[18] <- T
    params[19] <- "n_iter"
    params[20] <- nIter
    params[21] <- "iseed"
    params[22] <- .Paras$iseed
    params[23] <- "cluster_shape"
    params[24] <- clusterShape
    params[25] <- "NContexts"
    params[26] <- nContexts
    params[27] <- "ContextLengths"
    params[28] <- paste(contextLengths,sep=" ",collapse=" ")
    params[29] <- "NMaxAnnealingGlobal"
    params[30] <- .Paras$NMaxAnnealingGlobal
    params[31] <- "data_file"
    params[32] <- dataFileName
    params[33] <- "burn_in"
    params[34] <- burnIn
    params[35] <- "eliptical_within"
    params[36] <- elipticalWithin
    ##params[11] <- "missing"
    ##params[12] <- missing

	if(nChip > 0) {
		params[37]<-"bindingConsensus"
		params[38]<-.Paras$bindingConsensus
		params[39]<-"nChip"
		params[40]<-nChip
		params[41]<-"nConsensus"
		params[42]<-.Paras$nConsensus
		params[43]<-"consensusType"
		params[44]<-.Paras$consensusType
		params[45]<-"matrixThreshold"
		params[46]<-.Paras$matrixThreshold
		params[47]<-"chipThreshold"
		params[48]<-.Paras$chipThreshold
		params[49]<-"expressionRatio"
		params[50]<-.Paras$expressionRatio
		params[51]<-"binaryBinding"
		params[52]<-.Paras$binaryBinding
		params[53]<-"consensusClusteringType"
		params[54]<-.Paras$consensusClusteringType
		params[55]<-"KSTest"
		params[56]<-.Paras$KSTest
		params[57]<-"phase1"
		params[58]<-.Paras$phase1
		params[59]<-"cs_variance"
		params[60]<-.Paras$cs_variance
		params[61]<-"var_sigmas_x"
		params[62]<-.Paras$var_sigmas_x
		params[63] <- "EOF"
	}else {
		 #Zhen
		if(is.null(priorFile))
		{
			params[37] <- "EOF"
		}
		else
		{
			params[37] <- "isPriorModel"
			params[38] <- 'y'
			params[39] <- "isCategoryPrior"
			params[40] <- getPriorType(priorFile)
			params[41] <- "EOF"
		}	
	}
   
    ## End of File
    
    if (hostSystem!="Windows") cat(params, file="./parameters.prm", sep="\n")
    if (hostSystem=="Windows") cat(params, file="parameters.prm", sep="\n")

		dataFileName <- dataFile

		## If the data object is not provided then user must enter
		## filename with necessary path where data can be found.
		if(is.null(priorFile))
		{
			if (missing(tableData)) {
					geneDataTable <- convTxtToInp(dataFileName, M+nChip)
			}
			else {
					geneDataTable <- tableToInp(tableData, dataFileName, M+nChip)
			}
		}
		else
		{
			if (missing(tableData)) {
					geneDataTable <- convTxtAndPriorToInp(dataFileName, priorFile,M+nChip)
			}
			else {
					geneDataTable <- tableToInp(tableData, dataFileName,priorFile, M+nChip)
			}
		}

    if (is.na(geneDataTable[1,1])) {
        cat("Error in runGimmNPosthoc : Error in prepForGimm(), Exiting \n")
        return(FALSE)
    }

    ## Calling the executable
    cat("Running GIMM Executable ..........\n")
#print(.GimmPath)

	if(missing(clientID) & missing(host) & missing(port)) {
        fileName <- paste(.GimmPath, "gimm", sep="")
	} else {
	    fileName <- paste(.GimmPath, "gimm", " -i ", clientID, " -h ", host, " -p ", port, sep = "")
	}

#print(fileName)
#print(.Library)
    # Call the gimm executable
    system(fileName, intern=(!(verbose)) )

    commonParamsNData <- list(inputFile=dataFile, T=T, M=M, nChip=nChip,nSamples=nIter,
                          nreplicates=nreplicates, burnIn=burnIn,
                          verbose=verbose, intFiles=intFiles,
                          geneDataTable=geneDataTable)
   
##                              matrixOut=matrixOut, readingRaw=readingRaw, 
##                              probsOut=probsOut, clusterNumber=clusterNumber,
##                              clusterOption=clusterOption,

    return(commonParamsNData)
}
