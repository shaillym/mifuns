\name{nmcom}
\alias{nmcom}

\title{Calculate the Path for a NONMEM Execution Wrapper}
\description{
   Calculate the execution wrapper, given the installation directory
   for an NMQual-mediated NONMEM installation.
}
\usage{
nmcom(nmdir, pathsep = "/", ...)
}

\arguments{
  \item{nmdir}{}
  \item{pathsep}{}
  \item{\dots}{}
}
\details{
  Wrapper should be in sub directory 'test' and have same name as the
  installation directory.
}
\value{}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{runCommand}} }
\examples{
nmcom('/common/nonmem/nm6')
}


\keyword{manip}

