\name{rinvchisq}
\alias{rinvchisq}
\title{Generate inverse chi-square distribution}
\description{
  Generate inverse chi-square distribution given n, df, and covariance matrix
}
\usage{
rinvchisq(n, df, cov)
}
\arguments{
  \item{n}{the number of values to return}
  \item{df}{the degrees of freedom for the distribution. This value reflects the 
  uncertainity in the distribution.  A reasonable starting point would be a value 
  between the the number of subjects and the total number of observations used 
  to obtain the estimates of the variance-covariance matrix.}
  \item{cov}{a single parameter}
}
\value{
 The \code{rinvchisq} function returns \code{n} parameter deviates for use as simulation parameters.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\keyword{utilities}
