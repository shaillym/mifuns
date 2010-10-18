\name{iterations}
\alias{iterations}
\title{
Harvest Iteration Statistics from NONMEM Output and Convert from Covariance to Correlation
}
\description{
	\code{iterations} excises the relevant portion of the output file, converting
	the text to parameter and gradient estimates.
}
\usage{
iterations(x, ...)
}
\arguments{
  \item{x}{
character
}
  \item{\dots}{
ignored
}
}
\details{
The default result has intermediate dimensions as a compromise between 
very wide and very tall.
}
\value{
data frame indicating parameter estimates and gradients by iteration
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{cov2cor}}}
	}
}
\keyword{manip}

