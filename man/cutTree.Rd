\name{cutTree}
\alias{cutTree}
\title{ This function produces a new tree based upon the given probability.}
\description{
  This function cuts the given tree bease upon the height
  provided by the user. The dataset containing new tree
	is rebundled and returned to the user.
}
\usage{
	cutTree(gimmR, dataFile="Result", heat = FALSE, h = 0.05129625, nGenes)
}

\arguments{
  \item{gimmR}{
		This is the list of the all data returned after running gimm
		and posthoc related methods. This contains the hGClustData and
		clustData.
	}
  \item{dataFile}{
		The name of the file with extension .zm
	}
  \item{heat}{
		If true, the heatmap will be produced after cutting the tree
		at the given height
	}
  \item{h}{
		Height where the tree should be cut.
	}
  \item{nGenes}{
		Number of genes to be clustered.
	}
}

\details{
  This function cut the given tree bease upon the height
  provided by the user. User can see the heatmap of new
  tree by keeping the flag "heat" as true. 
  The dataset containing new tree is rebundled and
  returned to the user.In this list, clustData as well as
  hGClustData will change.
	
}
\author{ Mario Medvedovic, Vinayak Kumar}
\note{ Email Address : medvedm@ucmail.uc.edu }
\seealso{runGimmNPosthoc, runGimm, runPosthoc}
\keyword{ cluster }


