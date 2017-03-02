`runPosthocLc` <-
function (commonParamsNData, matrixOut="n", readingRaw="y", probsOut="y", clusterNumber=2, clusterOption="average")
{
	hostSystem <- Sys.info()["sysname"]
	if (missing(commonParamsNData)) {
		cat("Error in runOnlyPosthoc() : Parameter commonParamsNData missing")
		return(NA)
	}
	inputFile <- commonParamsNData$inputFile
	T <- commonParamsNData$T
	M <- commonParamsNData$M
	nChip <- commonParamsNData$nChip
	simpleContext <- commonParamsNData$sample
	nSamples <- commonParamsNData$nSamples
	nreplicates <- commonParamsNData$nreplicates
	burnIn <- commonParamsNData$burnIn
	pparams <- NULL
	if (nreplicates == 1) {
		readingY <- "n"
	} else {
		readingY <- "y"
	}
	inputFileName <- getOnlyFileName(inputFile)
	pparams[1] <- "matrix_out"
	pparams[2] <- matrixOut
	pparams[3] <- "probs_out"
	pparams[4] <- probsOut
	pparams[5] <- "T"
	pparams[6] <- T
	pparams[7] <- "M"
	pparams[8] <- M
	pparams[9] <- "reading_raw"
	pparams[10] <- readingRaw
	pparams[11] <- "reading_y"
	pparams[12] <- readingY
	pparams[13] <- "n_samples"
	pparams[14] <- nSamples
	pparams[15] <- "burn_in"
	pparams[16] <- burnIn
	pparams[17] <- "input_file"
	pparams[18] <- inputFileName
	pparams[19] <- "cluster_number"
	pparams[20] <- clusterNumber
	pparams[21] <- "cluster_option"
	pparams[22] <- clusterOption
	pparams[23] <- "simpleContext"
	if(is.null(simpleContext)) pparams[24] <- ""
	else pparams[24] <- simpleContext
	if (nChip > 0) {
		pparams[25] <- "consensusType"
		pparams[26] <- .Paras$consensusType
		pparams[27] <- nChip
		pparams[28] <- .Paras$nChip
		pparams[29] <- "EOF"
	} else {
		pparams[25] <- "EOF"
	}
	if (hostSystem != "Windows")
		cat(pparams, file="./posthoc_parameters.prm", sep="\n")
	if (hostSystem == "Windows")
		cat(pparams, file="posthoc_parameters.prm", sep="\n")
	cat("Running posthoc Executable ..... \n")
	fileName <- paste(.PosthocPath, "posthoc", sep="")
	if(is.null(simpleContext)) fileWithOption <- paste(fileName, " -l", sep="")
	else fileWithOption <- paste(fileName, " -L", sep="")
	system(fileWithOption, intern=(!(commonParamsNData$verbose)))
}

