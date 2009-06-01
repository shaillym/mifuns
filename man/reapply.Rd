\name{reapply}
\alias{reapply}
\title{Calculate Cell Aggregates Once per Cell Member}
\description{Calls \code{tapply()} but stretches the result to dimensions of \code{x}.}
\usage{
reapply(x, INDEX, FUN, ...)
}
\arguments{
  \item{x}{an atomic object, typically a vector}
  \item{INDEX}{list of factors, each of same length as \code{x}}
  \item{FUN}{the function to be applied. In the case of functions like \code{+}, \code{\%*\%}, etc., the function name must be quoted.}
  \item{\dots}{optional arguments to \code{FUN}}
}
\details{
The function \code{tapply()} applies \code{FUN} to each cell of a vector, as 
specified by levels of \code{INDEX}, normally returning one value for each cell. 
\code{reapply()} repeats that one value for each element of the cell, in an order 
corresponding to the original vector. Effectively, the result of \code{tapply()} but 
with the same length and order as \code{x}.
}
\value{array}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\examples{
Theoph$mean <- reapply(Theoph$conc,Theoph$Subject,'mean')
}
\keyword{manip}

