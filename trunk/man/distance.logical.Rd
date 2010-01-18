\name{distance.logical}
\alias{distance.logical}
\alias{distance}
\alias{ordinal}
\alias{enth}
\alias{ordinal.logical}
\alias{first}
\alias{first.logical}
\alias{unconditional}
\alias{unconditional.default}
\alias{unconditional.data.frame}
\alias{where}
\alias{where.logical}

\title{ Analyze Postions in a Condition.}
\description{
  For a \code{logical} vector, detect the n-th \code{TRUE} element, optionally for each
  level of the index \code{within}.  Analyze all elements with respect to 
  the detected element(s).
}
\usage{
%distance(x, ...)
%ordinal(x, ...)
\method{first}{logical}(x, within = rep(1, length(x)),...)
\method{ordinal}{logical}(x, n = 1, within = rep(1, length(x)),...)
\method{distance}{logical}(x, n = 1, within = rep(1, length(x)),...)
\method{where}{logical}(x, n = 1, within = rep(1,length(x)),...)
\method{unconditional}{default}(x,...)
\method{unconditional}{data.frame}(x,...)
}
\arguments{
  \item{x}{ a condition (\code{logical} vector). }
  \item{n}{ an integer giving the instance of interest (of \code{TRUE} within \code{x}). }
  \item{within}{ a vector or list of vectors serving as an index to \code{x}. }
  \item{\dots}{ ignored arguments. }

}
\details{
  \code{first.logical()} returns, for each element in \code{x}, (a copy of) the position 
  of the first occurence of \code{TRUE}, optionally within the index (\code{within}).
  
  \code{ordinal.logical()} is a generalization operating on the n-th element,
  rather than the first element.  \code{n} can be 0, returning all \code{NA}, or negative,
  which counts instances of \code{TRUE} from the end of the vector. \code{enth} is an
  alias for ordinal that perhaps more-readily suggests the concept "n-th".
  
  \code{distance.logical()} merely subtracts the \code{ordinal} of \code{x} from positions in \code{x}: giving the 
  (signed) distance of each element in \code{x} from the n-th \code{TRUE} (optionally within 
  levels of \code{within}).
  
  \code{where.logical()} identifies the n-th \code{TRUE} \code{within} each level, returning \code{logical}.
  
  \code{unconditional()} constructs an all-\code{TRUE} \code{logical} vector of appropriate length.
}
\value{
  \code{integer} (except \code{where.logical()}: \code{logical})
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{match}}}
	}
}

\examples{
test <- data.frame(
	id=c(1,1,1,1,1,2,2,2,2,2),
	time=c(1,2,3,4,5,1,2,3,4,5),
	evid=c(0,1,0,1,0,1,0,1,0,1)
)
test$secondDose  <- with(test,ordinal (evid==1, 2,within=id))
test$lastDose    <- with(test,ordinal (evid==1,-1,within=id))
test$beforeDose2 <- with(test,distance(evid==1, 2,within=id) < 0)
test$tafd <- with(test,time - time[ordinal(evid==1,1,within=id)])#time after first dose

#    id time evid secondDose lastDose beforeDose2 tafd
# 1   1    1    0          4        4        TRUE   -1
# 2   1    2    1          4        4        TRUE    0
# 3   1    3    0          4        4        TRUE    1
# 4   1    4    1          4        4       FALSE    2
# 5   1    5    0          4        4       FALSE    3
# 6   2    1    1          8       10        TRUE    0
# 7   2    2    0          8       10        TRUE    1
# 8   2    3    1          8       10       FALSE    2
# 9   2    4    0          8       10       FALSE    3
# 10  2    5    1          8       10       FALSE    4
}
\keyword{ manip }

