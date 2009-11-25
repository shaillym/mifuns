\name{nm.pl}
\alias{nm.pl}

\title{Build Commands to Invoke NONMEM}
\description{
 Formats a set of commandlines, suitable for submission to a command shell, to
 operate the renamed copy of 'nm.pl' in an NMQual-mediated NONMEM installation.
}
\usage{
nm.pl(nmdir, infile, outfile = NULL, perl = "perl", checksum = TRUE, compile = TRUE, execute = TRUE, split = FALSE, ...)
}

\arguments{
  \item{nmdir}{root of a NONMEM installation}
  \item{infile}{path for a control stream}
  \item{outfile}{path for output}
  \item{perl}{how to invoke Perl on this system}
  \item{checksum}{whether to conduct checksums}
  \item{compile}{whether to compile NONMEM}
  \item{execute}{whether to execute the compiled NONMEM}
  \item{split}{whether to compile and execute as separate steps}
  \item{\dots}{ignored}
}
\details{
  If split is TRUE, two commands are returned, having the mode flags 'c' and 'e', respectively.
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{help}} }
\examples{
nm.pl('/common/nonmem/nm6/','1.ctl')
nm.pl('/common/nonmem/nm6','1.ctl','1.out',search=FALSE,checksum=FALSE)
nm.pl('/nm6','1.ctl',execute=FALSE)
nm.pl('/nm6','1.ctl',compile=FALSE)
nm.pl('/nm6','1.ctl',split=TRUE)
}


\keyword{manip}

