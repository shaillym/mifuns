\name{filename}
\alias{filename}

\title{Construct a Canonical File Name}
\description{
 Helper function to build file and directory names from components.
}
\usage{
filename(dir, b = NULL, ext = NULL)
}

\arguments{
  \item{dir}{directory}
  \item{b}{run name}
  \item{ext}{file extension}
}
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{dataSynthesis}} }
\examples{
filename('dir','3''.ctl')
}
\keyword{manip}

