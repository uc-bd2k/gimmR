\name{splitMatrix}
\alias{splitMatrix}
\title{Split a matrix into sub matrices.}
\description{
  This function is similar to split() but returns a list of (sub-)matrices.
}
\usage{
splitMatrix(x, col = 1)
}
\arguments{
  \item{x}{ a matrix }
  \item{col}{ column defining the grouping of the matrix }
}
\value{
A list of sub matrices.
}
\author{Johannes Freudenberg}
\seealso{ \code{\link{split}} }
\examples{
x <- matrix(1:30, 6, 5)
x[,1] <- rep(1:3, times=2)
splitMatrix(x)
}
\keyword{cluster }
