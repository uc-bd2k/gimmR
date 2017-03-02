\name{runPosthoc}
\alias{runPosthoc}
\alias{runPosthocLc}
\title{
	This function prepares parameters for running executable
  "posthoc" and then exeecutes it. Use runPosthocLc to after 
  running localGeneClustering().
}
\description{
	Function to complete all the processes related to "posthoc".
  By calling this	function, all the operations regarding
  posthoc can be performed, but all the other processes 
  related to gimm are not be executed.
	
	User must provide the list data of all the parameters required
  for "posthoc" execution. This list will be available as
  the result of function "runGimm".
}
\usage{
runPosthoc (commonParamsNData, matrixOut="n", readingRaw="y",
            probsOut="y", clusterNumber=2, clusterOption="average", 
			estimate_contexts="n", clientID=-1, host=-1, port=-1)
}
\arguments{
  \item{commonParamsNData}{
   List data of parameters and data. Some of the parameters in
   this list will be used in this function. This list is the
   result produced by function "runGimm".
  }
  \item{matrixOut}{  
		Not used
		}
  \item{readingRaw}{  
		Not used
		}
  \item{probsOut}{
		Not used
		}
  \item{clusterNumber}{
		Number of clusters created by cutting the hierarchical clustering
		tree
		}
  \item{clusterOption}{
		Linkage principle to be used in constructing hierarchical clustering.
		"average" and "complete" are supported.
		 }
  \item{estimate_contexts}{
        "y" the context estimation GIMM was run.
		"n" the fixed contexts model or simple GIMM was run.
	  }
  \item{clientID}{
		The clientID that identifies the specific client to which the
		progress feedbacks are directed.
		This should be used only if feedback about progress need to be
		sent over network sockets.
	  }
  \item{host}{
	The hostname for the computer to which progress feedbacks
	should be sent. 
	This should be used only if feedback about progress need to be
	sent over network sockets.
	  }
  \item{port}{
	The port number to which progress feedbacks	should be sent. 
	This should be used only if feedback about progress need to be
	sent over network sockets.
	  }
}
\details{
	This function is designed to execute the processes related
  to "posthoc" only. It makes call to function "processPosthocParas()"
  using which parameter file called	"posthoc\_parameters.prm"
  are written. Then system call is made to executable "posthoc".
  User is supposed to provide the directory path in which these
  executables are residing. This can be done by changing the
  objects ".PosthocPath" in the library.

	In order to complete all the processings for Bayesian
  Clusttering algorithm, call to this function should be
  preceded by call to function "runGimm".

  This function provides the result via the .cdt and .gtr files.
}

\author{ Vinayak Kumar, Mario Medvedovic }
\note{ For additional information visit http://eh3.uc.edu/gimm }
\seealso{ runGimm }
\keyword{ cluster }


