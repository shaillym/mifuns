\name{contains}
\alias{contains}
\title{Test a Character Vector for Occurences of a Pattern}
\description{Test a character vector for occurences of a pattern.}
 \usage{
contains(pattern, text, ...)
}
\arguments{
  \item{pattern}{regular expression}
  \item{text}{\code{character} vector}
  \item{\dots}{extra arguments to regexpr()}
}
\details{
  Each element of text is tested for pattern, and returns \code{TRUE} if pattern matches.
}
\value{
\code{logical} vector as long as \code{text}
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{regexpr}}}
	}
}
\examples{
function(pattern,text,...){
	hits <- regexpr(pattern,text,...)
	hits >=0
 }
}
\keyword{manip}
