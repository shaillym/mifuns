\name{bracket}
\alias{bracket}
\title{Create an XML Tag}
\description{
  Create an open or close XML tag of the form \code{<x>} or \code{</x>}.
}
\usage{
bracket(x, close = FALSE, ...)
}
\arguments{
  \item{x}{an element name}
  \item{close}{whether the tag is a close tag.  FALSE by default.}
  \item{\dots}{Name-value pairs of attributes for an open tag.}
}
\details{
  \code{close} can be a vector.  An attempt will be made to supress attributes
  for 'close' tags.
}
\value{
 character
}
\references{ mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{
	\itemize{
		\item{}{\code{\link{attribute}}}
		\item{}{\code{\link{as.xml.character}}}
	}
}
\examples{
bracket(rep('name',2),close=FALSE:TRUE,row=1,col=2)
}
\keyword{ manip }

