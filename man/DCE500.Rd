\name{DCE500}
\alias{DCE500}
\docType{data}
\title{Gene expression dataset containing top 500 differentially coexpressed genes.}
\description{
  This dataset was derived from the Schmidt et al. (2008) primary breast tumor data set by identifying the top 500 differentially co-expressed genes using the DCIM algorithm.
}
\usage{data(DCE500)}
\format{
  A data frame with 500 observations on the following 202 variables.
}
\details{
 see our support page http://eh3.uc.edu/gimm/dcim/ 
}
\source{
 http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE11121
}
\references{
 Schmidt M, Böhm D, von Törne C, Steiner E et al. The humoral immune system has a key prognostic impact in node-negative breast cancer. Cancer Res 2008 Jul 1;68(13):5405-13.
 http://www.ncbi.nlm.nih.gov/sites/entrez?Db=Pubmed&term=18593943[UID]
}
\examples{
data(DCE500)
gimmOut <- runGimmNPosthoc(DCE500, dataFile="DCE500", M=200, T=500, estimate_contexts="y", intFiles=FALSE, verbose=TRUE, nIter=500, burnIn=250)
drawHeatmap(gimmOut, scale="none")
}
\keyword{datasets}
