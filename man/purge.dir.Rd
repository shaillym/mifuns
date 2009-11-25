\name{purge.dir}
\alias{purge.dir}

\title{Purge a Directory}
\description{
 Purge a directory, perhaps nicely.
}
\usage{
purge.dir(dir, nice = FALSE)
}

\arguments{
  \item{dir}{a directory}
  \item{nice}{whether to purge system files}
}
\details{
  If 'nice' is TRUE, system files are not removed (as defined for all.files in dir()).
}
\value{Used for side effects.}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{runNonmem}} }
\keyword{manip}

