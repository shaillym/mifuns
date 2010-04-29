\name{half.matrix}
\alias{half.matrix}
\alias{half}
\title{
Vectorize a Triangular Representation of a Symmetric Matrix
}
\description{
Given a symmetric matrix, the corresponding lower (upper) triangular
matrix is converted to a vector in row- (column-) major order.  The vector 
is named using the convention \code{row.column} (for the lower interpretation).
}
\usage{
\method{half}{matrix}(x)
}
\arguments{
  \item{x}{
symmetric matrix
}
}
\details{
It is an error if \code{x} is not symmetric.
}
\value{
vector with as many elements as a triangular matrix corresponding to x
}
\references{ http://mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{ 
	\code{\link{ord.matrix}}, 
	\code{\link{isSymmetric.matrix}}
}
\examples{
half(diag(3))
}
}
\keyword{ manip }

