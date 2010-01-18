\name{diagnosticPlots}
\alias{diagnosticPlots}
\alias{covariatePlots}
\alias{cwresPlots}
\title{Create Diagnostic Plots}
\description{
  Create diagnostic plots from a data set.
}
\usage{
  diagnosticPlots(
        data, 
	dvname='DV', 
	group=NULL, 
	model=NULL, 
	...
  )

  covariatePlots(
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	par.list=NULL, 
	eta.list=NULL, 
	...
  )

  cwresPlots(
	data,
	cont.cov=NULL, 
	cat.cov=NULL,
	...
  )
}
\arguments{
  \item{data}{A data.frame, typically created by dataSynthesis().}
  \item{dvname}{Name of the dependent variable to use as a label for the diagnostic plots.}
  \item{group}{Optionally, a column name in \code{data} used to condition the output of diagnosticPlots().}
  \item{model}{If supplied, this text argument will be prepended to the figure titles.}
  \item{cont.cov}{As defined for PLOTR.}
  \item{cat.cov}{As defined for PLOTR.}
  \item{par.list}{As defined for PLOTR.}
  \item{eta.list}{As defined for PLOTR.}
  \item{\dots}{ignored arguments}
}
\details{
PLOTR() passes the result of dataSynthesis() to these functions and prints the
resulting lists (of trellis objects) on the open device.
}
\value{
a list of trellis objects
}
\references{http://metruminstitute.org; mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{PLOTR}}}
		\item{}{\code{\link{dataSynthesis}}}
	}
}
\keyword{manip}

