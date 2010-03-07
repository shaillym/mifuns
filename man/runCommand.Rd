\name{runCommand}
\alias{runCommand}

\title{Issue the System Call That Invokes NONMEM}
\description{
   If udef is FALSE, \code{command} is passed to nm.pl() with supporting arguments.  If grid
  is TRUE, the result is passed to qsub() with supporting arguments.  If boot
  is TRUE, '&' is appended to the result.  Each element of the result is executed
  in succession using a system call customized by platform.
}
\usage{
runCommand(
	command,
	run,
	rdir,
	boot,
	urgent,
	checksum,
	grid,
	udef = FALSE,
	ctlfile,
	outfile,
	perl = if (nix()) "perl -S" else if (!invisible) "cmd /K perl -S" else "cmd /C perl -S",
	intern = invisible,
	minimized = invisible,
	invisible = FALSE,
	split = grid,
	N = paste("Run", run, if (split) c("c", "e") else NULL, sep = ""),
	o = rdir,
	e = rdir,
	L=if(split)c(compileflag(compiler(config(dirname(command)))),NA)else NA,
	hold_jid = if (split) c(NA, paste("Run", run, "c", sep = "")) else NA,
	V = "",
	j = "y",
	q=if(split)
	    c(
	        'compile.q',
	        if(urgent)'all.q' else 'bootstrap.q'
	    )
	else 
	    if(!execute)
	        'compile.q'
            else if(urgent)'all.q' else 'bootstrap.q',
	sync = if (boot) "n" else "y",
	shell = "n",
	b = "y",
	cwd = "",
	compile = TRUE,
	execute = TRUE,
	background='&',
	...
)
}
\arguments{
  \item{command}{a command to pass to system()}
  \item{run}{run name}
  \item{rdir}{run directory}
  \item{boot}{see NONR}
  \item{urgent}{see NONR}
  \item{checksum}{see NONR}
  \item{grid}{see NONR}
  \item{udef}{see NONR}
  \item{ctlfile}{see runNonmem}
  \item{outfile}{see runNonmem}
  \item{perl}{a character string to invoke perl}
  \item{intern}{see NONR, passed to system()}
  \item{minimized}{see NONR, passed to system()}
  \item{invisible}{see runNonmem}
  \item{split}{whether compile and execute should be run separately}
  \item{N}{passed to qsub}
  \item{o}{passed to qsub}
  \item{e}{passed to qsub}
  \item{L}{passed to qsub as an instance of 'l'}
  \item{hold\_jid}{passed to qsub}
  \item{V}{passed to qsub}
  \item{j}{passed to qsub}
  \item{q}{passed to qsub}
  \item{sync}{passed to qsub.  Bootstrap runs usually occur in large quantities,
  whereas SGE has an internal limitation on number of synchronized processes.  Therefore,
  no attempt is currently made to synchronize bootstrap runs.}
  \item{shell}{passed to qsub}
  \item{b}{passed to qsub}
  \item{cwd}{passed to qsub}
  \item{compile}{passed to nm.pl}
  \item{execute}{passed to nm.pl, influences default for \code{q}}
  \item{background}{appended to bootstrap command lines to put the 
  process in the background}
  \item{\dots}{passed to nm.pl and qsub}
}
\details{
The argument 'L' represents a possibly-constituative instance of qsub's 'l',
but is not called 'l' so that other instances of 'l' (multiple are allowed)
will not accidentally override it.  Users can override intentionally, of course.

N, L, and hold\_jid are coordinated that if a run is split, compile status is flagged
on the compile run, and the execute run waits for compile to finish.

'q' is handled specially.  When overriding, be sure to pass a character vector of
length one for a normal run, and of length two if split is TRUE (the default when
grid is TRUE).  By default, all standalone compile-only runs are diverted to 
'compile.q', as well as all compile halves of split runs.  By default, all execute-only
runs as well as the execute halves of split runs are diverted to 'all.q' if urgent,
and 'bootstrap.q' otherwise.
}
\value{Used for side effects}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{runNonmem}}}
	}
}
\keyword{manip}

