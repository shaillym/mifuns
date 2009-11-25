\name{groupnames}
\alias{groupnames}

\title{Construct a Single Vector of Group Names from Possibly Multiple Indicies to the Plotting Data Set}
\description{
 Callers of PLOTR may wish to group on more than one index.  This function combines
 those indicies into text for a single index vector.
}
\usage{
groupnames(data, grp, grpnames = NULL, b)
}

\arguments{
  \item{data}{data.frame in which to find the groups}
  \item{grp}{columns by which to group}
  \item{grpnames}{optional substitute names}
  \item{b}{run name}
}
\value{factor}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{ \code{\link{PLOTR}} }
\keyword{manip}

