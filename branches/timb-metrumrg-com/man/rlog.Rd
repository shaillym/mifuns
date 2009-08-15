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
	boot=FALSE, 
	ProjectDir=getwd(), 
	append=TRUE,
	file=NULL,
	out=filename(ProjectDir,'CombRunLog.csv'),
	pattern=if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	...
)
}
\arguments{
  \item{b}{ Vector of control stream numbers to use to generate a combined run log.
  Should be numeric with no placeholder zeros to left of number, i.e., "1" not "001".
  Example: \code{b=c(1,2,5)} }
  \item{boot}{ Were these runs in '.boot' directories?}
  \item{ProjectDir}{System path to the directory containing the NONMEM control (*.ctl) streams. }
  \item{append}{TRUE builds the runlog by appending; FALSE causes the runlog to 
  be overwritten.}
  \item{file}{The run-specific input file to read. See details.}
  \item{out}{The run-generic output file to write. See details.}
  \item{pattern}{Search patterns for files to delete (regular expressions).}
  \item{\dots}{Extra arguments, currently ignored.}
}

\details{
  \code{file} is ProjectDir/*/NonmemRunLog.csv by default.  '*', if present, will be replaced 
  with run number when \code{file} is user-specified.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\keyword{ manip }
