\name{rinvgamma}
\alias{rinvgamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rinvgamma(n, df, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{df}{ ~~Describe \code{df} here~~ }
  \item{sigma}{ ~~Describe \code{sigma} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(n,df,sigma) {
# shape=alpha=DF
# rate = beta
# mode(rinvgamma)=beta/(alpha+1)
# mean(rinvgamma)=beta/(alpha-1)
# var(rinvgamma)=beta^2/(alpha-1)^2/(alpha-2)

	shape <- df
	rate <- sigma*df
	return(1/rgamma(n, shape, rate))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

