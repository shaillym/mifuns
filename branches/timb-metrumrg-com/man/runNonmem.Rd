\name{runNonmem}
\alias{runNonmem}

\title{Process a Request for NONMEM Invocation}
\description{
 The heart of NONR, this function handles file-level details and dispatches 
 the other major functions, especially runCommand and PLOTR.
}
\usage{
runNonmem(
	command, 
	ProjectDir, 
	run, 
	boot, 
	urgent, 
	checkrunno, 
	diag, 
	fdata, 
	epilog, 
	dvname, 
	logtrans, 
	grp, 
	grpnames, 
	cont.cov, 
	cat.cov, 
	par.list, 
	eta.list, 
	missing, 
	invisible, 
	checksum, 
	grid, 
	nice, 
	udef, 
	file, 
	ctlfile = NULL, 
	outfile = NULL, 
	tabfile = NULL, 
	msffile = NULL, 
	parfile = NULL, 
	outdir = ProjectDir, 
	pattern = c("^F[ISRC]", "^OU", "^nonmem.exe", if (fdata) c("^FD", "^PR")), 
	...
)
}
\arguments{
  \item{command}{see NONR}
  \item{ProjectDir}{see NONR}
  \item{run}{see NONR}
  \item{boot}{see NONR}
  \item{urgent}{see NONR}
  \item{checkrunno}{see NONR}
  \item{diag}{see NONR}
  \item{fdata}{see NONR}
  \item{epilog}{see NONR}
  \item{dvname}{see NONR}
  \item{logtrans}{see NONR}
  \item{grp}{see NONR}
  \item{grpnames}{see NONR}
  \item{cont.cov}{see NONR}
  \item{cat.cov}{see NONR}
  \item{par.list}{see NONR}
  \item{eta.list}{see NONR}
  \item{missing}{see NONR}
  \item{invisible}{see NONR}
  \item{checksum}{see NONR}
  \item{grid}{see NONR}
  \item{nice}{see NONR}
  \item{udef}{see NONR}
  \item{file}{see NONR}
  \item{ctlfile}{see dataSynthesis}
  \item{outfile}{see dataSynthesis}
  \item{tabfile}{see dataSynthesis}
  \item{msffile}{see dataSynthesis}
  \item{parfile}{see dataSynthesis}
  \item{outdir}{see dataSynthesis}
  \item{pattern}{regular expressions for files to purge}
  \item{\dots}{extra arguments for other functions}
}
\value{Used for side effects}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{NONR}},\code{\link{NONR}},runCommand }
\keyword{manip}

