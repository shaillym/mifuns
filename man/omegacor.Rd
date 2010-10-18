\name{omegacor}
\alias{omegacor}
\title{
Convert NONMEM Omega Covariance Matrix to Correlation Matrix
}
\description{
This function operates on a character vector read from a NONMEM output (.lst)
file.  It finds the block of text representing final parameter estimates for 
Omega.  It harvests these and converts from covariance matrix to correlation 
matrix.
}
\usage{
omegacor(x, ...)
}

\arguments{
  \item{x}{
character vector representing NONMEM output file (*.lst)
}
  \item{\dots}{
other arguments, unused
}
}
\value{
correlation matrix
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{cov2cor}}}
	}
}
\keyword{manip}

