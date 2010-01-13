\name{runlog}
\alias{runlog}
\title{Create a Run Log for NONMEM7 using NONMEM6 Format}
\description{
  MIfuns includes an INFN routine for NONMEM6 and earlier that creates a 
  comma-separated listing of parameter estimates and their percent relative
  standard errors.  This function emulates that format, using the '.ext' file
  produced under NONMEM7.
}
\usage{
runlog(
	run, 
	outfile, 
	estfile = file.path(dirname(outfile), 
	paste(run, "ext", sep = ".")), 
	runlogName = "NonmemRunLog.csv", 
	...
)
}
\arguments{
  \item{run}{name of the NONMEM run, typically integer}
  \item{outfile}{path for the NONMEM output file, typically 'run.lst'}
  \item{estfile}{path for the NONMEM estimates, by default 'run.ext'}
  \item{runlogName}{path and name for file to create}
  \item{\dots}{ignored}
}
\details{
  The 'ext' file is digested.  In particular the rows labeled -1000000000
  and -1000000001 are taken as the parameter estimates and standard errors,
  respectively.  The minimization status is either 0 or 1, which is less 
  informative than for NONMEM6.  The covariance status is represented as blank:
  it is left to the user to to determine covariance status.
}
\value{
 used for side effects
}
\note{
For automated log generation, call NONR with \code{epilog=runlog}.
}
\references{ mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{\code{\link{read.param}}}
\keyword{ manip }

