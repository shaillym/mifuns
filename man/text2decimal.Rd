\name{text2decimal}
\alias{text2decimal}
\title{Convert Mixed Text to Decimal}
\description{Convert \code{character} to a \code{numeric} value where possible.}
\usage{
text2decimal(x)
}
\arguments{
  \item{x}{\code{character}}
}
\details{
\code{x} is coerced to \code{character}; leading/trailing text is dropped; and the 
result is coerced to \code{numeric}. Useful where measurements are reported with units. 
Leading text is characters not among \code{0:9}, \code{+},\code{-}, or \code{.}. The numeric region
may have a leading sign, must have one or more digits (\code{0:9}), may have a decimal point,
and may have any number of trailing digits.
}
\value{numeric}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\examples{
text2decimal('every 3.5 hours')
text2decimal('-17 units')
}
\keyword{manip}

