############################################################
## Name    : callPosthoc()
## Purpose : Alternative to runPosthoc() that is compatible
##           with runGimmNPosthoc() function call.
## Author  : Johannes Freudenberg
############################################################
`callPosthoc` <-
function(tableData, dataFile, M, T, nChip=0, nIter=10000, nreplicates=1,
	contextSpecific="y", nContexts=1, contextLengths=M, estimate_contexts="n",
	clusterShape="v", burnIn=5000, elipticalWithin="n", matrixOut="n",
	readingRaw="y", probsOut="y", clusterNumber=2, clusterOption="average", 
	verbose=FALSE, intFiles=FALSE) 
{
	reqParamsList <- list(inputFile=dataFile, T=T, M=M, nChip=nChip, 
		nSamples=nIter, nreplicates=nreplicates, burnIn=burnIn, verbose=verbose, 
		intFiles=intFiles, geneDataTable=tableData)
	runPosthoc(reqParamsList, matrixOut, readingRaw, probsOut, clusterNumber, 
		clusterOption, estimate_contexts)
}
