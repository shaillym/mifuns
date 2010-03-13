\name{rlog}
\alias{rlog}
\title{Generate a combined run log from multiple NONMEM runs}
\description{
  Generates a combined run log across specified runs.  Appends/overwrites 
  existing file.  Attempts to purge certain files in run directory.  
}
\usage{
rlog(
  	run, 
	boot = FALSE, 
	project = getwd(), 
	append = TRUE,
	tool = 'nm6',
	file = filename(project,'CombRunLog.csv'),
	rundir = filename(project, run, if(boot) '.boot' else ''),
	nmlog = file.path(rundir,'NonmemRunLog.csv'),
	nmout = filename(rundir,run,'.lst'),
	pattern = if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	...
)
}
\arguments{
  \item{run}{ vector of control stream names (numbers)}
  \item{boot}{Were these runs in '.boot' directories?}
  \item{project}{path to the directory containing run subdirectories}
  \item{append}{TRUE: append file; FALSE: overwrite file}
  \item{tool}{'nm6' or 'nm7': controls methodology}
  \item{file}{The run-generic output file to write. See details.}
  \item{rundir}{path to run subdirectory}
  \item{nmlog}{The run-specific input file to read. See details.}
  \item{pattern}{Search patterns for files to delete (regular expressions).}
  \item{\dots}{Extra arguments, currently ignored.}
}

\details{
  rundir, nmlog, and nmout can be vector; or scalar, in which case they may 
  contain '*', to be replaced with run names on a per-run basis. 
  
  To suppress file deletion, supply a zero-length argument such as NULL for 
  'pattern'.
  
  To suppress log creation on disk, supply a zero length argument for 'file'.
}
\value{
  Combined runlog is returned invisibly in the 'unilog' format.  Side effect: if
  file is specified, 'runlog' format is written to disk.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\seealso{
	\itemize{
		\item{}{\code{\link{as.unilog.run}}}
		\item{}{\code{\link{as.runlog.file}}}
	}
}
\keyword{ manip }
