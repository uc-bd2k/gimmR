############################################################
## Name    : getPerm()
## Author  : Mario Medvedovic
############################################################
#getPerm<-function(merge, clusterSize, level,start,perm){
#	leftChild<-merge[level,1]
#	rightChild<-merge[level,2]
#	leftSize<-0
#	rightSize<-0
#	if(leftChild < 0) {
#		perm[start]<-(-leftChild)
#		leftSize<-1
#	}else {
#		leftSize<-clusterSize[leftChild]	
#		perm<-getPerm(merge, clusterSize,leftChild,start,perm)
#	}
#
#	if(rightChild < 0) {
#		perm[start+leftSize]<-(-rightChild)
#	}else {
#		perm<-getPerm(merge, clusterSize,rightChild,start+leftSize,perm)	
#	}
#
#	perm
#}
## iterative version to avoid stack overflow
## Author  : Johannes M Freudenberg
getPerm<-function(merge, clusterSize, level, start, perm) {
	perm <- nrow(merge)
	allLeafs <- FALSE
	while (!allLeafs) {
		allLeafs <- all(perm < 0)
		if (!allLeafs) {
			index <- which(perm >= 0)[1]
			newPerm <- as.vector(merge[perm[index], ])
			if (index > 1)
				newPerm <- c(perm[1:(index - 1)], newPerm)
			if (index < length(perm))
				newPerm <- c(newPerm, perm[(index + 1):length(perm)])
			perm <- newPerm
		}
	}
	-perm
}
