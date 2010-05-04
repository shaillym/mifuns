\name{half.matrix}
\alias{half.matrix}
\alias{print.halfmatrix}
\alias{as.matrix.halfmatrix}
\alias{half}
\title{
Interconvert Between a Symmetric Matrix and a Triangular Matrix
}
\description{
Given a symmetric matrix, \code{half.matrix} converts the corresponding lower (upper) triangular
matrix to a vector in row- (column-) major order.  The vector 
is named using the convention \code{row.column} (for the lower interpretation)
and is classed as \code{halfmatrix}.  The function \code{as.matrix.halfmatrix}
converts a halfmatrix-like vector to a symmetric matrix.
}
\usage{
\method{half}{matrix}(x,...)
\method{as.matrix}{halfmatrix}(x,...)
}
\arguments{
  \item{x}{symmetric matrix (half) or halfmatrix equivalent (as.matrix)}
  \item{\dots}{extra arguments, ignored}
}
\details{
It is an error if \code{x} is a matrix but not symmetric. If \code{x} is a 
vector of appropriate length it can be converted to a symmetric matrix by specifying the method
explicitly, even if \code{x} is not classed as halfmatrix:  \code{as.matrix.halfmatrix(x)}.
}
\value{
vector with as many elements as a triangular matrix corresponding to x (half), 
or matrix (as.matrix)
}
\references{ http://mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{ 
	\code{\link{ord.matrix}}, 
	\code{\link{isSymmetric.matrix}}
}
\examples{
half(diag(3))
as.matrix(half(diag(3)))
as.matrix.halfmatrix(1:15)
}
\keyword{ manip }

