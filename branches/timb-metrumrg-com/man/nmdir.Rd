\name{nmdir}
\alias{nmdir}

\title{Recover an Installation Directory from the Path to a NONMEM execution Wrapper}
\description{
 Trims the wrapper path to just the installation directory.
}
\usage{
nmdir(nmcom, pathsep = "/", ...)
}

\arguments{
  \item{nmcom}{NONMEM command (file path)}
  \item{pathsep}{file system path separator}
  \item{\dots}{ignored}
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{runCommand}} }
\examples{
nmdir('/common/nonmem/nm6/test/nm6.pl')
}
\keyword{manip}

