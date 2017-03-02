\name{deleteFiles}
\alias{deleteFiles}
\title{ Function that deletes intermediate files upon user's request.}
\description{
	This function deletes the intermediate file generated after
	running gimm and posthoc. Here the user can choose the files
	to be deleted. The files should reside in current folder
}
\usage{
deleteFiles(dataFile, zm=FALSE, pos=FALSE, mpm=FALSE, verbose=FALSE)
}
\arguments{
\item{dataFile}{
		User should provide the Filename. Only the exact name of the file
		will be used without any extension. if missing, default File name
		is "Result"
	}
\item{verbose}{
		If true, all the internal comments of the commands 
    will be displayed on console.
	}
\item{zm}{
		If true, .zm file will be deleted. Default is false.

	}
\item{mpm}{
		If true, .mpm file will be deleted. Default is false.

	}
\item{pos}{
		If true, .pos file will be deleted. Default is false.
	}
}
\value{
}
\author{ Vinayak Kumar, Mario Medvedovic}
\seealso{cleanFolder}
\keyword{ cluster }


