\name{fixProblem}
\alias{fixProblem}
\alias{fixFile}
\alias{explicitPath}
\alias{extractPath}
\alias{resolve}
\alias{scavenge}
\alias{tabfile}
\alias{parfile}
\alias{msffile}
\alias{extfile}
\title{Clean Up a Control Stream}
\description{
   These functions are used to re-write a control stream for 
   \code{NONR(checkrunno=TRUE,...)}.
}
\usage{
fixProblem(x,run)
fixFile(x,run)
explicitPath(x)
extractPath(x)
resolve(file,dir)
scavenge(expr,lines)
extfile(ctlfile,dir,extreg,...)
tabfile(ctlfile,dir,tabreg='(?<!par)\\\\.tab',...)
parfile(ctlfile,dir,parreg='par\\\\.tab',...)
msffile(ctlfile,dir,msfreg='^(?!\\\\$MSFI).*\\\\.msf',...)
}

\arguments{
  \item{x}{character vector, i.e. read from control stream file}
  \item{run}{run designator to substitute at predefined locations}
  \item{file}{an absolute or relative filepath}
  \item{dir}{a directory}
  \item{expr}{a regular expression to locate relevant lines}
  \item{lines}{lines in which to search for expr}
  \item{ctlfile}{character vector representing the control stream}
  \item{extreg}{a regular expression to locate an arbitrary file}
  \item{tabreg}{a regular expression to locate the .TAB file}
  \item{parreg}{a regular expression to locate the par.TAB file}
  \item{msfreg}{a regular expression to locate the .MSF file}
  \item{\dots}{ignored}
}
\details{
fixProblem() tries to substitute the run name in the NONMEM \$PROBLEM statement
with the current run name.  It looks for PROB or PROBLEM, and then RUN or RUN\# 
(optional).  The next space delimited word is replaced.

fixFiles() tries to replace the run name in filenames ending in .msf, par.tab,
or .tab.  Currently it relies on dirname() and basename(), which use 
the same file separator on 'Nix and Windows.  basename() fails if there is no 
path separator, e.g. FILE=40.tab.  fixFiles() pre-processes its argument with
explicitPath as a work-around.  Lines containing MSFI will be coerced to explicit
paths, but will not otherwise be substituted.

explicitPath() finds lines that contain .tab or .msf but not '/'.  It tries to 
place './' before run.msf or run.tab.

extractPath() isolates the file portion of a line of text.  It consists of a sequence
of nonspace characters following MSF=, MSFO=, or FILE=.  Spaces may occur
before and after the equality sign.  This function is currently used by runNonmem()
to locate the tabfile, parfile, and msffile.

resolve() does nothing to absolute filepaths in 'file', but expresses others (those
beginning with '.') relative to 'dir'.

tabfile(), parfile(), and msffile() calculate corresponding filepaths from their
arguments, relying on extfile() as the common engine.
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{runNonmem}}}
		\item{}{\code{\link{dataSynthesis}}}
	}
}
\examples{
prob <- c(
	'$PROBLEM  1000 desc',
	'$PROB  1000 desc',
	'$PROBLEM  RUN1000 desc',
	'$PROB  RUN#1000 desc',
	'$PROB  RUN# 1000 desc'
)
fixProblem(prob,2000)
msf <- c(
	'$EST MAXEVAL=9999 MSF=../1000.MSF',#standard
	'$EST MAXEVAL=9999 msf=../1000.MSF',#case change
	'$EST MAXEVAL=9999 MSF =  ../1000.MSF',#one or two spaces
	'$EST MAXEVAL=9999 MSFO=../1000.MSF',#optional O
	'$EST MAXEVAL=9999 MSF=../1000.msf',#case change
	'$EST MAXEVAL=9999 MSF=../1000.msf INTER',#trailing text
	'$EST MAXEVAL=9999 MSF=../1000/1000.msf',#non-target path elements
	'$EST MAXEVAL=9999 MSF=1000.msf',#no path
	'$MSFI 1000.MSF',#non-target file name
	'$INPUT etc'#non-target
)
fixFile(msf,2000)
tab <- c(
	'ONEHEADER NOPRINT FILE=../1000.TAB',
	'ONEHEADER NOPRINT FILE  = ../1000.TAB',
	'ONEHEADER NOPRINT FILE=../1000par.TAB',
	'ONEHEADER NOPRINT file=../1000par.tab',
	'ONEHEADER NOPRINT file=../1000par.tab',
	'ONEHEADER NOPRINT file= 1000par.tab',
	'ONEHEADER NOPRINT FILE=../1000.TAB',
	'ONEHEADER NOPRINT FILE=~/1000.TAB'
)
fixFile(tab,2000)
files <- c(
	'1000.msf',
	'1000.tab',
	'./1000.tab',
	'../1000.tab',
	'$TABLE EVID FILE=1000.tab',
	'$TABLE EVID FILE=1000par.tab',
	'$MSFI 1000.msf'
)
explicitPath(files)
extractPath(msf)
extractPath(tab)
resolve('./file.ext',dir='project/pk')
resolve('../file.ext',dir='project/pk/')
scavenge('.tab',lines=c('text file.TAB','text file.csv'))
ctlfile <- c('text file=../100.tab etc','text file=../100par.tab etc','text file=../100.msf etc')
f <- textConnection(ctlfile)
tabfile(f,dir='projectdir/100')
close(f)

f <- textConnection(ctlfile)
parfile(f,dir='projectdir/100')
close(f)

f <- textConnection(ctlfile)
msffile(f,dir='projectdir/100')
close(f)
}		
\keyword{manip}

