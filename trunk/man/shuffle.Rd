\name{shuffle}
\alias{shuffle}
\title{
Move the columns of a data.frame relative to eachother.
}
\description{
It can be tedious to reorder the columns in a data.frame.  This function lets
you move specific columns relative to some other named column.
}
\usage{
shuffle(x, who, after = NA)
}
\arguments{
  \item{x}{
data.frame
}
  \item{who}{
a character vector of column names to move
}
  \item{after}{
character:  the column after which to put 'who'
}
}
\details{
If \code{after} is NA, the named columns are moved to the front (before the first column).
}
\value{
data.frame: a variant of x
}
\references{
mifuns.googlecode.com
}
\author{
Tim Bergsma
}
\seealso{
\code{\link{match}}
}
\examples{
head(Theoph)
head(shuffle(Theoph,'Time'))
}
\keyword{ manip }
