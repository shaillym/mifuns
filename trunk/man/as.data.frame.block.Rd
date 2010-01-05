\name{as.data.frame.block}
\alias{as.data.frame.block}
\title{Convert a Block of Lines to Data Frame}
\description{
  A block is a character vector where each element represents a line of text
  from a table.  This function converts the character vector to a data frame.
  The first line must be a header, and names will not be checked.
}
\usage{
as.data.frame.block(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ character }
  \item{\dots}{ extra arguments passed to read.table()}
}
\details{
  The function calls \code{read.table} with header=TRUE, as.is=TRUE, and 
  check.names=FALSE. It is an error to specify these arguments.
}
\value{
  data.frame
}
\references{ mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{\code{\link{read.table}}}
\examples{
as.data.frame.block(
	c(
		'just an example',
		'run param estimate',
		'1 CL 45',
		'1 V2 70',
		'1 Ka 3.14'
	)
)
}
\keyword{ manip }

