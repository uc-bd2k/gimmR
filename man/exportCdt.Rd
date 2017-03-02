\name{exportCdt}
\alias{exportCdt}
\alias{exportGtr}
\alias{exportAtr}
\title{Functions to generate fTreeView files}
\description{
exportCdt, exportGtr, exportAtr, are slightly modified of the r2cdt, r2gtr, and r2atr functions, respectively,  from the package ctc.  

exportCdt generates a tab-delimited cdt file from a dataframe where the first column contains gene IDs, the second column contains gene names or descriptions, and the remaining columns represent samples.  Column names are assumed to be sample names.  In addition, corresponding hierarchical gene and sample clusterings are required to re-order genes and samples accordingly.  

exportGtr and exportAtr generate tab-delimited TreeView files representing the hierarchical gene and sample clustering, respectively. 

}
\usage{
exportCdt(hr, hc, data, labels = TRUE, description = TRUE, file = "cluster.cdt", dec = ".")
exportGtr(hr, file = "cluster.gtr", dec = ".")
exportAtr(hc, file = "cluster.atr", dec = ".")
}
\arguments{
  \item{hr}{An hclust object representing the gene clustering}
  \item{hc}{An hclust object representing the sample clustering}
  \item{data}{A dataframe where the first column may contain gene IDs, the second column may contain gene names or descriptions, and the remaining columns represent samples.  Column names are assumed to be sample names. }
  \item{labels}{If TRUE, the first column of data is assumed to contain gene IDs.}
  \item{description}{If TRUE, the second column of data is assumed to contain gene names or descriptions.}
  \item{file}{The name of the file to be generated.}
  \item{dec}{The character to be used for decimal points.}
}
\details{
}
\value{
Nothing is returned but a file with the specified name is created.
}
\references{ctc package: http://antoinelucas.free.fr/ctc/

fTreeView: http://eh3.uc.edu/ftreeview}
\author{Johannes Freudenberg}
\note{  
}
\seealso{\code{\link[ctc:ctc-package]{ctc}}, \code{\link{hclust}}}
\examples{
data(GalData)
gimmOut <- runGimmNPosthoc(GalData, dataFile="galData", M=20, T=820, estimate_contexts="y", intFiles=TRUE, verbose=TRUE)
exportCdt(gimmOut$hGClustData, gimmOut$hSClustData, gimmOut$clustData)
exportGtr(gimmOut$hGClustData)
exportAtr(gimmOut$hSClustData)
}
\keyword{ IO }
\keyword{ cluster }
