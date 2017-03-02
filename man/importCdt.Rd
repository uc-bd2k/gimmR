\name{importCdt}
\alias{importCdt}
\alias{importAtr}
\alias{importGtr}
\title{Functions to import files in TreeView format into R.}
\description{
  importCdt() imports a cdt file and converts it to a dataframe where rows represent genes and columns represent samples.  The first column of the dataframe contains the gene identifiers and the second column contains gene names or descriptions.  Column names represent sample names.
}
\usage{
importCdt(cdtFile)
importGtr(gtrFile)
importAtr(atrFile)
}
\arguments{
  \item{cdtFile}{A cdt file}
  \item{gtrFile}{A gtr file}
  \item{atrFile}{An atr file}
}
\details{
  importCdt() reads a tab-delimited file in generalized cdt format where the first row contains sample names, followed by additional optional rows.  The first two or three columns contain gene IDs and descriptions, and optional additional columns, followed by gene expression data.  The function returns a dataframe with gene IDs and descriptions in the first two columns, gene expression data in the remaining columns, and column names representing samples. 
  importGtr() and importAtr() read tree files representing the hierarchical gene tree or sample tree structure, respectively.  Both functions return an hclust object.
}
\value{
  importCdt() returns a dataframe (see details)
  importGtr() and importAtr() each return an hclust objects
}
\references{ coming soon }
\author{ Johannes Freudenberg, Xiangdong Liu, Mario Medvedovic }
\seealso{ \code{\link{hclust}}\link{exportCdt}} 
\examples{
data(GalData)
gimmOut <- runGimmNPosthoc(GalData, dataFile="galData", M=20, T=820, estimate_contexts="y", intFiles=TRUE, verbose=TRUE)
exportCdt(gimmOut$hGClustData, gimmOut$hSClustData, gimmOut$clustData)
exportGtr(gimmOut$hGClustData)
exportAtr(gimmOut$hSClustData)
exprData <- importCdt("cluster.cdt")
gclust <- importGtr("cluster.gtr")
plot(as.dendrogram(gclust))
sclust <- importAtr("cluster.atr")
}
\keyword{ cluster }
\keyword{ IO }
