`generateLocalPPPs` <-
function(dataFile, M, T, samples=NULL, cc=NULL, out=NULL, wcS=NULL, nIter=1000, burnIn=500, verbose=TRUE) {
	if(length(samples)==1) {
		reqParamsList <- list(inputFile=dataFile, T=T, M=M, sample=samples, nChip=0, nSamples=nIter, 
			nreplicates=1, burnIn=burnIn, verbose=verbose, intFiles=FALSE, geneDataTable=NULL)
	} else {
		nIter <- localGeneClustering(dataFile, samples=samples, out=out, wcS=wcS, burnIn=burnIn, cc=cc)
		reqParamsList <- list(inputFile=dataFile, T=T, M=M, nChip=0, nSamples=nIter, nreplicates=1, burnIn=0,
			verbose=verbose, intFiles=FALSE, geneDataTable=NULL)
	}
	runPosthocLc(reqParamsList, matrixOut="n", readingRaw="y", probsOut="y", clusterNumber=2, clusterOption="average")
}

