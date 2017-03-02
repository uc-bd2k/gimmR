############################################################
## Name    : drawHeatmap()
## Purpose : This function produces a heatmap for the given
##           gimmR object.
## Author  : Vinayak Kumar, Mario Medvedovic
############################################################

"drawHeatmap" <-
function (gimmRObj, color="red-green", ...) {

	library(marray)
	if (color == "blue-yellow"){
			pal <- maPalette(low="yellow", high="blue", mid="black")
	}
	else {
			pal <- maPalette(low="green", high="red", mid="black")
	}
	hGClustData <- gimmRObj$hGClustData
	clustData <- gimmRObj$clustData

	if(is.null(gimmRObj$hSClustData)) colv <- NA
	else colv <- as.dendrogram(gimmRObj$hSClustData)
	
	heatmap(as.matrix(clustData[,3:(ncol(clustData))]), labRow=clustData[,1], Rowv=as.dendrogram(hGClustData), col=pal, Colv=colv, ...)

}

