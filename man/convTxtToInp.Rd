\name{convTxtToInp}
\alias{convTxtToInp}
\title{ This function converts the Gene data in Txt file to Inp format.}
\description{
This function is used to produce the .Inp file from the .txt file of Gene Data. The executable "gimm" uses this ".inp" file to get the informations regarding the Genes. It takes filename as input argument, using which the ".txt" file is searched. This ".txt" file should be in the current working folder.
}
\usage{
convTxtToInp(fileName = "", M)
}
\arguments{
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
To execute this function properly, ".txt" file should be present in the current working directory.
}
\value{
  This functions returns a table read from the txt file. If the .txt file was not available then NULL will be returned.
}
\author{ Vinayak Kumar, Mario Medvedovic}
\note{ Email Address : kumarvk@ucmail.uc.edu }
\seealso{processPostHocParas, runGimmNPosthoc, processGimmParas}
\keyword{ cluster }

