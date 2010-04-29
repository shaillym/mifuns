\name{ord.matrix}
\alias{ord.matrix}
\alias{ord}
\title{
Give the Order of a Symmetric Matrix
}
\description{
The dimensions of a symmetric matrix are equal; this value is, by definition,
the order of the matrix.
}
\usage{
\method{ord}{matrix}(x, ...)
}
\arguments{
  \item{x}{
symmetric matrix
}
  \item{\dots}{
extra arguments, ignored
}
}
\details{
It is an error if \code{x} is not symmetric.
}
\value{
scalar numeric
}
\references{ http://mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{ 
	\code{\link{is.square.matrix}}, 
	\code{\link{isSymmetric.matrix}}
}
\examples{
ord(diag(4))
}
}
\keyword{ manip }

