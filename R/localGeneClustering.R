`localGeneClustering` <-
function(dataFile, out=NULL, wcS=NULL,  cc=NULL, samples=NULL, burnIn=500) {
##defining local functions
	withinContexts.Contexts2Samples <- function(x, contextClusters, filename, burnIn) { 
		x <- as.numeric(strsplit(x, split="\t")[[1]])
		if(x[1] < burnIn) {
			rm(x)
		} else {
			if(x[1] %%100 == 0 & x[2] == 0) cat(paste("\n   iteration ", x[1], " ... "))
			y <- contextClusters[as.numeric(x[1])+1,]
			y <- c(x[1:2],x[match(y, 0:(length(x)-1)) + 2])
			write.table(t(y), file=filename, sep="\t", col.names=FALSE, row.names=FALSE, append=TRUE)
			rm(x,y)
		}
		NULL
	}
	Global2Local <- function(localClusterMapping, geneClusters, filename, samples) {
		y <- geneClusters[localClusterMapping[1,1]+1,]
		if(localClusterMapping[1,1] %%100 == 0) cat(paste("\n   iteration ", localClusterMapping[1,1], " ... "))
		y <- t(localClusterMapping[match(y, localClusterMapping[,2]), samples + 2])
		byContext <- apply(y, 1, paste, collapse="\t")
		y <- levels(as.factor(byContext))
		write.table(cbind(table(byContext), y), file=filename, sep="\t", col.names=FALSE, row.names=FALSE, append=TRUE, quote=FALSE)
		l <- length(y)
		rm(y, byContext)
		l
	}
	Global2Local_single <- function(localClusterMapping, geneClusters, sample) {
		y <- geneClusters[localClusterMapping[1,1]+1,]
		if(localClusterMapping[1,1] %%100 == 0) cat(paste("\n   iteration ", localClusterMapping[1,1], " ... "))
		y <- localClusterMapping[match(y, localClusterMapping[,2]), sample + 2]
		c(1, y)
	}
## end local functions
	if(is.null(out)) {
		cat("Reading .out file ... ")
		out <- as.matrix(read.table(paste(dataFile, "out", sep="."), header=FALSE, stringsAsFactors=FALSE))
		cat("done.\n")
	}
	if(is.null(wcS)) {
		if(file.exists(paste(dataFile, "wcs", sep="."))) {
			if(as.POSIXlt(file.info(paste(dataFile, "wcs", sep="."))$mtime) > as.POSIXlt(file.info(paste(dataFile, "out", sep="."))$mtime)) {
				cat("Reading .wcs file ... ")
			} else {
				file.remove(paste(dataFile, "wcs", sep="."))
			}
		}
		if(!file.exists(paste(dataFile, "wcs", sep="."))) {
			cat("Reading .cc file ... ")
			if(is.null(cc)) {
				cc <- as.matrix(read.table(paste(dataFile, "cc", sep="."), header=FALSE, stringsAsFactors=FALSE))
			} else {
				nIter <- dim(out)[1]
				cc <- matrix(rep(cc, each=nIter), nIter)
			}
			cat("done.\nReading .wc file ... ")
			wc <- as.matrix(read.table(paste(dataFile, "wc", sep="."), header=TRUE, stringsAsFactors=FALSE, sep="+"))
			cat("done.\n")
			file.create(paste(dataFile, "wcs", sep="."))
			cat("Converting contexts to samples ... ")
			sapply(wc[,1], withinContexts.Contexts2Samples, contextClusters=cc, filename=paste(dataFile, "wcs", sep="."), burnIn=burnIn)
		}
		wcS <- as.matrix(read.table(paste(dataFile, "wcs", sep="."), header=FALSE, stringsAsFactors=FALSE))
		cat("done.\n")
	}
	
	if(file.exists(paste(dataFile, "lc", sep="."))) file.remove(paste(dataFile, "lc", sep="."))
	file.create(paste(dataFile, "lc", sep="."))
	if(is.null(samples)) { 
		samples <- 1:(dim(wcS)[2]-2)
		warning(paste("Parameter samples not specified.  Using all", length(samples), "samples."))
	} else if(length(setdiff(samples, 1:(dim(wcS)[2]-2))) > 0) {
		warning(paste("Parameter samples must be a subset of 1:", dim(wcS)[2]-2, ".", sep=""))
	}
	if(length(samples) > 1) {
		cat("Converting global clusters to local clusters ... ")
		out.Local <- lapply(splitMatrix(wcS), Global2Local, geneClusters=out, filename=paste(dataFile, "lc", sep="."), samples=samples)
		cat("done.\n")
	} else {
		cat("Splitting wcS matrix ... ")
		wcS <- splitMatrix(wcS)
		cat("done.\n")
		cat("Converting global clusters to local clusters ... ")
		out.Local <- lapply(wcS, Global2Local_single, geneClusters=out, sample=samples)
		cat("done.\n")
		#cat("Unlist out.Local ... ")
		#nRow <- length(out.Local)
		#out.Local <- unlist(out.Local)
		#cat("done.\n")
		#cat("Converting to matrix ... ")
		#out.Local <- matrix(out.Local, nrow=nRow, byrow=TRUE)
		#cat("done.\n")
		#cat("Writing output ... ")
		#write.table(out.Local, file=paste(dataFile, "lc", sep="."), 
		#	sep="\t", col.names=FALSE, row.names=FALSE, append=TRUE, quote=FALSE)
		#cat("done.\n")
		cat("writing output ... ")
		nRow <- length(out.Local)
		filename <- paste(dataFile, "lc", sep=".")
		lapply(out.Local, function(x) write.table(matrix(x, nrow=1), file=filename, sep="\t", col.names=FALSE, row.names=FALSE, append=TRUE, quote=FALSE))
		cat("done.\n")
		out.Local <- nRow 
	}
	sum(unlist(out.Local))
}

