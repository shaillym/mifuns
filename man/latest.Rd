\name{latest}
\alias{latest}
\title{Identify the Latest Variants of Each File as Distinguished by Enclosing
Subdirectories}
\description{
  For each of a set of file variants as returned by \code{variants}, identify
  that file whose enclosing directory sorts last (i.e., latest if a timestamp).
}
\usage{
latest(x)
}
\arguments{
  \item{x}{character vector of file paths of the form 'path/file/var1/file'}
}
\value{
 character
}
\references{ mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{\code{\link{variants}}}
\examples{
latest(
	c(
		'path/file/var2/file',
		'path/file/var1/file'
	)
)
}
\keyword{ manip }

