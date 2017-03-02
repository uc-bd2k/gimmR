## List of parameters which aren't generally changed by user
.Paras <- 
  list(Q=1, alpha=1, average="n", automatic_annealing="y",
			 iseed=1, NMaxAnnealingGlobal=1, bindingConsensus='y',
			 consensusType='onlyChip',expressionRatio=0.7,consensusClusteringType=2,KSTest='n',
			 KSTest='n',cs_variance='y',var_sigmas_x='y',binaryBinding='n',chipThreshold=2.0,nConsensus=0,matrixThreshold=2.0,phase1=0)

## Current folder as the defualt path for executables
#".GimmPath" <- paste(.Library, "/gimmR/doc/", sep="")
#print(.Library)
#print(.GimmPath)

#.PosthocPath <- paste(.Library, "/gimmR/doc/", sep="")


## This contains the Table object for the Original data
.GeneDataTable<- NA

## The below statement does not work on R 2.14 because it seems R 
## enforces locks on certain fields. Altered on : Tue Feb 28 15:47:59 EST 2012
# ".GeneDataTable" <- NA
# ".GeneDataTable" <<- NA

############################################################
## Name    : runGimmNPosthoc()
## Purpose : This is Top Layer function for running gimm 
##					 and posthoc
## Author  : Vinayak Kumar
############################################################

runGimmNPosthoc <-
function(tableData, dataFile, M, T, nChip=0, nIter=10000, nreplicates=1,
				 contextSpecific="n", nContexts=1, contextLengths=NA, estimate_contexts="n",
				 clusterShape="v", burnIn=5000, elipticalWithin="n", matrixOut="n",
				 readingRaw="y", probsOut="y", clusterNumber=2,
  				 clusterOption="average", verbose=FALSE, intFiles=FALSE, clientID=-1, host=-1, port=-1, priorFile=NULL) {

	if(missing(clientID) & missing(host) & missing(port)) {
		## Call runGimm() for Processing related to gimm
         reqParamsList <- runGimm(tableData, dataFile, M, T, nChip,nIter, nreplicates,
 	        contextSpecific, nContexts, contextLengths, estimate_contexts, 
     		clusterShape, burnIn, elipticalWithin, verbose, intFiles,priorFile=priorFile);
	    ## Call runPosthoc for processing Posthoc related tasks
         runPosthoc(reqParamsList, matrixOut, readingRaw, probsOut,
		    clusterNumber,clusterOption, estimate_contexts)
	} else {

        reqParamsList <- runGimm(tableData, dataFile, M, T, nChip,nIter, nreplicates,
	        contextSpecific, nContexts, contextLengths, estimate_contexts, 
    		clusterShape, burnIn, elipticalWithin, verbose, intFiles, clientID, host, port, priorFile=priorFile);
        runPosthoc(reqParamsList, matrixOut, readingRaw, probsOut,
		    clusterNumber, clusterOption, estimate_contexts, clientID, host, port)
	}
}
