\name{simblock}
\alias{simblock}
\title{Generate omega values for simulation}
\description{
  Generate random effect values given the number of simulations, degrees of freedom, and
  a variance covariance matrix (block).
}
\usage{
simblock(n, df, cov)
}
\arguments{
  \item{n}{the number of simulations that will be performed}
  \item{df}{vector of degrees of freedom of block elements}
  \item{cov}{variance-covariance matrix}
}
\details{
  \code{df} and \code{cov} should have the same length.
}
\value{
 matrix, with one row per simulation and one column per element in the corresponding
triangular version of \code{cov}. 
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Leonid Gibianski, Bill Knebel, Tim Bergsma). }
\seealso{
	\itemize{
		\item{}{\code{\link{simpar}}}
	}
}
\keyword{ manip }
