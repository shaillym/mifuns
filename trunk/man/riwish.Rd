\name{riwish}
\alias{riwish}
\title{Generate inverse Wishart distribution}
\description{
 Generates an inverse Wishart distribution for a variance of a random effect.
 The functions requires the specification of degrees of freedom, scale matrix, 
 and precision for each random effect.
}
\usage{
riwish(s, df, prec)
}
\arguments{
  \item{s}{scale matrix for inverse Wishart distribution}
  \item{df}{degrees of freedom}
  \item{prec}{presision matrix: the mean of riwish() is proportional to \code{prec}}
}
\value{
 The variance of the random effect can be drawn from the returned inverse Wishart distribution.
 }
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Leonid Gibiansky, Bill Knebel, Tim Bergsma, and others). }
\keyword{ manip }
