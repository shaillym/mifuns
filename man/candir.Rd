\name{candir}
\alias{candir}

\title{Calculate A Canonical Directory Name}
\description{
 Strip trailing path separator from a directory name, if present.
}
\usage{
candir(dir, pathsep = "/")
}

\arguments{
  \item{dir}{character representing a directory name}
  \item{pathsep}{character representing the file system path separator}
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{runCommand}} }
\examples{
candir('/common/directory/')
candir('c:\\common\directory\\',pathsep='\\\\')
}


\keyword{manip}

