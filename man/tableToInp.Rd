\name{tableToInp}
\alias{tableToInp}
\title{ Function to convert Gene data table object to Inp format.}
\description{
	This function is used to produce the .Inp file from
	the Gene Data table object. The executable "gimm"
	uses this ".inp" file to get the informations 
	regarding the Genes. 
}
\usage{
tableToInp(geneDataTable, fileName, M)
}
\arguments{
  \item{geneDataTable}{
	Using this parameter, the Data table object can be
	passed to this function. If this object is absent,
	an error is thrown.
 }
  \item{fileName}{
	This parameter is used to provide the file name 
	of the .inp file which will be created from the 
	table object provided to this function.
}
  \item{M}{
		Dimensionality of the expression vectors to be clustered. I.e. the number
		of microarray.
	}
}
\details{
}
\value{
  This functions returns the same table object which
	is passed to this function.
}
\author{ Vinayak Kumar, Mario Medvedovic}
\note{ Email Address : kumarvk@ucmail.uc.edu }
\seealso{processPostHocParas, runGimmNPosthoc, processGimmParas}
\keyword{ cluster }

