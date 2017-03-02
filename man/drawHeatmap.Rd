\name{drawHeatmap}
\alias{drawHeatmap}
\title{This function draws a heatmap based on given gimmR object.}
\description{
  This function draws a heatmap based on given gimmR object.
	User can provide the two types of color scheme, either
	"blue-yellow" or "red-green".
}
\usage{
	drawHeatmap(gimmRObj, color="red-green", ...)
}

\arguments{
  \item{gimmRObj}{
		This is the list of the all data returned after running gimm
		and posthoc related methods. This contains the hGClustData and
		clustData.
	}
  \item{color}{
		Color scheme provided by user. It can be either "red-green"
		or "blue-yellow". Default is "red-green".
	}
  \item{...}{
		Additional parameters passed on to heatmap().
	}
}
\author{Mario Medvedovic, Vinayak Kumar}
\note{Email Address : medvedm@ucmail.uc.edu }
\seealso{runGimmNPosthoc, runGimm, runPosthoc}
\keyword{ cluster }

