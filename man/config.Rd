\name{config}
\alias{config}

\title{Identify the Configuration File in an NMQual-mediated NONMEM installation.}
\description{
   Gives the path for the configuration file in a NONMEM installation directory.
}
\usage{
config(nmdir, pathsep = "/", ...)
}
\arguments{
  \item{nmdir}{root directory of an NMQual-mediated NONMEM installation}
  \item{pathsep}{file system path separator}
  \item{\dots}{ignored}
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{help}} }
\examples{
config('/common/nonmem/nm6')
}
\keyword{manip}

