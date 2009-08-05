\name{rlog}
\alias{rlog}
\title{ Generate a combined run log from multiple NONMEM runs }
\description{
  rlog will generate a combined run log for all run numbers
  requested.  rlog will append to or overwrite any existing
  combined run log depending on the user options specified 
  in the call to rlog.  On 'Nix systems, certain files are 
  gratuitously purged, especially for bootstrap directories.
}
\usage{
rlog(
  	b, 
	boot=0, 
	ProjectDir=getwd(), 
	append=TRUE,
	file=NULL,
	out=NULL,
	...
)
}
\arguments{
  \item{b}{ Vector of control stream numbers to use to generate a combined run log.
  Should be numeric with no placeholder zeros to left of number, i.e., "1" not "001".
  Example: \code{b=c(1,2,5)} }
  \item{boot}{ Flag to describe type of NONMEM run with (0)-normal, (1)-bootstrap, 
  (2)-long runs, and (3)-bootstrap runs across entire cluster. Coerced to integer,
  so FALSE/TRUE give 0/1.  Should be "0" on Windows. }
  \item{ProjectDir}{System path to the directory containing the NONMEM control (*.ctl) streams. }
  \item{append}{TRUE builds the runlog by appending; FALSE causes the runlog to 
  be overwritten.}
  \item{file}{The run-specific input file to read. See details.}
  \item{out}{The run-generic output file to write. See details.}
  \item{\dots}{Extra arguments, currently ignored.}
}

\details{
  \code{file} is ProjectDir/*/NonmemRunLog.csv by default.  \code{out}
  is ProjectDir/CombRunLog.csv by default.  '*', if present, will be replaced 
  with run number when \code{file} is user-specified.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\keyword{ manip }
