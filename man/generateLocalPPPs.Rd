\name{generateLocalPPPs}
\alias{generateLocalPPPs}
\alias{localGeneClustering}
\title{Wrapper function to compute local pairwise posterior probabilities given a set of samples (context) }
\description{
  After running gimm and posthoc with the estimate_context option set to "y" and the intFiles option set to "TRUE", the user may specify a set of samples (called context) to compute the local pairwise posterior probabilities for all pairs of genes.
}
\usage{
generateLocalPPPs(dataFile, M, T, samples = NULL, cc = NULL, out = NULL, wcS = NULL, nIter=1000, burnIn = 500, verbose = TRUE)
localGeneClustering(dataFile, out = NULL, wcS = NULL, cc = NULL, samples = NULL, burnIn = 500)
}
\arguments{
  \item{dataFile}{ file name used in previous gimm/posthoc run }
  \item{M}{ total number of samples }
  \item{T}{ total number of genes }
  \item{samples}{ a set of samples.  must be a subset of (1:M) }
  \item{cc}{ if specified, a matrix of sample to context assignments where rows represent Gibbs steps and columns represent samples. }
  \item{out}{ if specified, a matrix of global gene cluster assignments where rows represent Gibbs steps and columns represent genes. }
  \item{wcS}{ if specified, a matrix of local gene cluster assignments where rows represent Gibbs steps as well as global clusters and columns represent samples. }
  \item{nIter}{ nIter used in previous gimm/posthoc run.}
  \item{burnIn}{ burnIn used in previous gimm/posthoc run.}
  \item{verbose}{ want progress updates? }
}
\details{
  The function first creates a file similar to the .out file, then runs the C function posthoc.  The output (i.e. the local PPPs) is stored in a .zm3 file.
}
\value{
An error code ('0' for success) produced when running the C function "posthoc."
}
\references{ http://eh3.uc.edu/gimm}
\author{ Johannes Freudenberg}
\seealso{ \code{\link{runGimmNPosthoc}}, \code{\link{computeDCEscore}}}
\examples{
data(GalData)
gimmOut <- runGimmNPosthoc(GalData, dataFile="galData", M=20, T=820, estimate_contexts="y", intFiles=TRUE, verbose=TRUE)
generateLocalPPPs(dataFile="galData", M=20, T=820, samples=which(cutree(gimmOut$hSClustData, k=2)==1), nIter=10000, burnIn=5000)
localPPPs <- read.table("galData.zm3")[, 1]
localPPPs <- matrix(localPPPs, 820, 820)
localGtr <- hclust(as.dist(1 - localPPPs), method = "average")
localGtr$labels <- 1:820
plot(localGtr)
}
\keyword{ cluster}
