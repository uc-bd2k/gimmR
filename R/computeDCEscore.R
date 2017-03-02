`computeDCEscore` <-
function(context1=NULL, context2=NULL, 
	paramsList=list(dataFile="Result", M=15, T=200, burnIn=5000),
	localPPPs1=NULL, localPPPs2=NULL, localGtr1=NULL, localGtr2=NULL, maxSize=1000, minSize=10, verbose=TRUE)
{
	if(!(is.null(context1) | is.null(context2))) {
		if(verbose) 
		    cat("Contexts specified. Computing local PPPs (this may take a while)...\n")
		if(missing(paramsList)) 
		    warning("paramsList not specified. Using default list.")
		generateLocalPPPs(dataFile=paramsList$dataFile, M=paramsList$M, T=paramsList$T, samples=context1, burnIn=paramsList$burnIn, verbose=verbose)
		localPPPs1 <- read.table(paste(paramsList$dataFile, "zm3", sep="."))[,1]
		localPPPs1 <- matrix(localPPPs1, sqrt(length(localPPPs1)), sqrt(length(localPPPs1)))
		generateLocalPPPs(dataFile=paramsList$dataFile, M=paramsList$M, T=paramsList$T, samples=context2, burnIn=paramsList$burnIn, verbose=verbose)
		localPPPs2 <- read.table(paste(paramsList$dataFile, "zm3", sep="."))[,1]
		localPPPs2 <- matrix(localPPPs2, sqrt(length(localPPPs2)), sqrt(length(localPPPs2)))
    } else {
		if(is.null(localPPPs1) | is.null(localPPPs2)) 
			stop("Either (context1,context2) or (localPPPs1,localPPPs2) must be specified.")
	}
	if(is.null(localGtr1)) {
		if(verbose) 
		    cat("Generating hierarchical clustering for context 1 (this may take a while)...\n")		
		localGtr1 <- hclust(as.dist(1-localPPPs1), method="average")
		localGtr1$labels <- 1:nrow(localPPPs1)
	}
	if(is.null(localGtr2)) {
		if(verbose) 
		    cat("Generating hierarchical clustering for context 2 (this may take a while)...\n")		
		localGtr2 <- hclust(as.dist(1-localPPPs2), method="average")
		localGtr2$labels <- 1:nrow(localPPPs2)
	}
	getPerGeneDiffCoExprScore <- function(gtr, PPPs1, PPPs2, nGenes=10000, maxSize=1000, minSize=10) {
		allClusters <- getAllClusters(gtr, maxSize=maxSize, minSize=minSize)
		f <- function(subs, PPPs1, PPPs2, nGenes) {
			m <- rep(0, nGenes)
			l <- length(subs)
			m[subs] <- rowMeans(PPPs1[subs,subs] - PPPs2[subs, subs])*l/(l-1)
			abs(m)
		}
		geneMatrixPPPsDif <- matrix(unlist(lapply(allClusters, f, PPPs1=PPPs1, PPPs2=PPPs2, nGenes=nGenes)), nGenes)
		maxPPPsDif <- apply(geneMatrixPPPsDif, 1, max)
		maxPPPsDif
	}
	maxPPPsDif_1 <- getPerGeneDiffCoExprScore(localGtr1, localPPPs1, localPPPs2, nGenes=nrow(localPPPs1), maxSize=maxSize, minSize=minSize)
	maxPPPsDif_2 <- getPerGeneDiffCoExprScore(localGtr2, localPPPs1, localPPPs2, nGenes=nrow(localPPPs2), maxSize=maxSize, minSize=minSize)
	invisible(apply(cbind(maxPPPsDif_1, maxPPPsDif_2), 1, max))
}

