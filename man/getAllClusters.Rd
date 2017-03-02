\name{getAllClusters}
\alias{getAllClusters}
\title{Generate a list of all possible clusters from a hierarchical tree.}
\description{
  The function take an hclust object and returns a list of all possible clusters of desired minimum to maximum size. Clusters are determined by cutting the tree at a given level and recording the leaf node labels of the resulting subtree(s).
}
\usage{
getAllClusters(tree, maxSize = 1000, minSize = 10, clusterList = NULL, k=NULL)
}
\arguments{
  \item{tree}{ a hierarchical clustering (hclust object) }
  \item{maxSize}{ maximum cluster size }
  \item{minSize}{ minimum cluster size }
  \item{clusterList}{ depricated. kept for backward compatibilty. }
  \item{k}{Maximum number of clusters. If specified, the list of clusters will be restricted to the top k clusters within the minSize-maxSize range starting at the root.}
}
\details{
}
\value{
  A list where each list element is a vector of leaf nodes described by the labels slot of the original tree (hclust object).
}
\author{ Johannes Freudenberg}
\seealso{ \code{\link{hclust}}}
\examples{
gtr <- hclust(dist(1:10))
gtr$labels <- 1:10
plot(gtr)
getAllClusters(gtr, 10, 2)
}
\keyword{cluster }
