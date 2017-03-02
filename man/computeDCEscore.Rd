\name{computeDCEscore}
\alias{computeDCEscore}
\title{A wrapper function to compute the DCE score given two contexts.}
\description{
  After running gimm and posthoc with the estimate_context option set to "y" and the intFiles option set to "TRUE", the user may specify two sets of samples (called contexts) to first compute the local pairwise posterior probabilities for all pairs of genes for both contexts and then compute a gene-specific differential co-expression score reflecting the differences between the two contexts.
}
\usage{
computeDCEscore(context1 = NULL, context2 = NULL, paramsList = list(dataFile = "Result", M = 15, T = 200, burnIn = 5000), localPPPs1 = NULL, localPPPs2 = NULL, localGtr1 = NULL, localGtr2 = NULL, maxSize = 1000, minSize = 10, verbose = TRUE)
}
\arguments{
  \item{context1}{a set of samples.  must be a subset of (1:M).}
  \item{context2}{a set of samples.  must be a subset of (1:M).}
  \item{paramsList}{List of paramters used in previous gimm/posthoc run.}
  \item{localPPPs1}{A matrix of local PPPs}
  \item{localPPPs2}{Another matrix of local PPPs}
  \item{localGtr1}{Hierarchical local gene clustering }
  \item{localGtr2}{Another hierarchical local gene clustering}
  \item{maxSize}{Maximum gene cluster size when computing gene-specific score. }
  \item{minSize}{Minimum gene cluster size when computing gene-specific score. }
  \item{verbose}{How much is too much information?}
}
\details{
  If context1 and context2 are specified, local PPPs are computed for these contexts.  Otherwise localPPPs1 and localPPPs2 are used directly.  
  Pre-computed local gene clusterings (localGtr1, localGtr2) are used if provided, otherwise they are derived from the local PPPs. 
  Next, local hierarchical gene clusterings are split into lists of gene clusters which are then used to determine the gene-specific DCE score.
}
\value{
A vector of length T representing the DCE score.
}
\references{ http://eh3.uc.edu/gimm }
\author{Johannes Freudenberg}
\seealso{\code{\link{runGimmNPosthoc}}}
\examples{
data(GalData)
gimmOut <- runGimmNPosthoc(GalData, dataFile="galData", M=20, T=820, estimate_contexts="y", intFiles=TRUE, verbose=TRUE)
res <- computeDCEscore(which(cutree(gimmOut$hSClustData, k=2)==1), which(cutree(gimmOut$hSClustData, k=2)==2), paramsList=list(dataFile="galData", M=20, T=820, burnIn=5000))
res <- res[gimmOut$hGClustData$order]
plot(1:length(res), res, type="n")
polygon(c(1:length(res), length(res), 1), c(res, 0, 0), col=4)
}
\keyword{cluster}
