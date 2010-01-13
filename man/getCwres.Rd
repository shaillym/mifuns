\name{getCwres}
\alias{getCwres}

\title{Return the Conditional Weighted Residuals Pertinent to a Given Run}
\description{
 Locates cwtab1.deriv and calls compute.cwres().  Then returns the CWRES column
 from 'cwtab1'.
}
\usage{
getCwres(directory)
}

\arguments{
  \item{directory}{NONMEM run directory}
}
\value{numeric}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{PLOTR}}, \code{\link{NONR}} }
\keyword{manip}

