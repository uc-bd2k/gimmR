############################################################
## Name    : cutTree()
## Purpose : This function produces a new tree based upon
##           the given probability.
## Author  : Vinayak Kumar, Mario Medvedovic
############################################################

"cutTree" <-
function (gimmR, dataFile="Result", heat=FALSE, h=0.05129625, nGenes) {

  ## Get the filname of datafile without any extension
  dataFileName <- getOnlyFileName(dataFile)

	#importing zm files
	zmData<-scan(file= paste("./", dataFileName, ".zm", sep=""),  sep="\t",
                     multi.line=FALSE, flush=FALSE,  skip=0) 
	distMatrix<-matrix(1-zmData, nrow=nGenes, byrow=TRUE)

	# Depending on the height given, cut the tree
	CSclustering<-cutree(gimmR$hGClustData, h=h)

	#picking up genes that are co-clustered with at least one or more gene
	TableCSclustering<-table(CSclustering)
	CSClusters<-dimnames(TableCSclustering[TableCSclustering > 1])
	OrfsCSClusters<-!is.na(match(as.character(CSclustering), unlist(CSClusters)))

	## sum(OrfsCSClusters)
	## ngenes<-sum(OrfsCSClusters)
	## ngenes

	#subsetting the distance matrix
	ppsmOrfsCSClusters<-distMatrix[OrfsCSClusters,OrfsCSClusters]

	## dim(ppsmOrfsCSClusters)

	#creating new distance matrix
	dppsmOrfsCSClusters<-as.dist(ppsmOrfsCSClusters)

	#creating new clustering
	hcdppsmOrfsCSClusters<-hclust(dppsmOrfsCSClusters,method="average")

	#subsetting the data matrix gimmR$clustData
	dataOrfsCSClusters<- gimmR$clustData[OrfsCSClusters,]
	
	## dim(dataOrfsCSClusters)


	###creat new gimmR and if heat=T display heatmap
	newGimmRData <- list (hGClustData=hcdppsmOrfsCSClusters,
												clustData=dataOrfsCSClusters, dataFile=dataFileName,
												labels=dataOrfsCSClusters[,1])

	## now for heatmap
	## Contains all the numeric rows for the data
  ## and the gene names for the Row labeling

	if (heat) {

		drawHeatmap(newGimmRData)
##		heatmap(as.matrix(dataOrfsCSClusters[,3 : (ncol(dataOrfsCSClusters)) ] ),
##						labRow = dataOrfsCSClusters[,1],
##						Rowv=as.dendrogram(hcdppsmOrfsCSClusters))
	}

#	heatmap(as.matrix(dataOrfsCSClusters[,3 : (ncol(dataOrfsCSClusters)) ] ),
#          labRow = dataOrfsCSClusters[,1])


	## Return the newGimmRData as result
	return (newGimmRData)
}

