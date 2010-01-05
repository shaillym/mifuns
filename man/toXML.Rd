\name{toXML}
\alias{toXML}
\alias{toXML.default}
\alias{toXML.character}
\alias{toXML.data.frame}
\title{Represent an Object as XML}
\description{
  \code{toXML} is generic, with a default method and methods for \code{character}
  and \code{data.frame}.  The intent is to convert text-like objects to informal
  XML that can be navigated with XPath expressions, e.g. using package XML.
}
\usage{
\method{toXML}{default}(x, tag, ...)
\method{toXML}{character}(x, tag, ...)
\method{toXML}{data.frame}(x, ...)
}
\arguments{
  \item{x}{an object to convert}
  \item{tag}{an XML element name for open and close tags in which to enclose 
  members of x; can be a vector}
  \item{keyname}{an XML element name to hold each value of each column in x}
  \item{key}{a vector of unique identifiers to distinguish each value of each column
  in x}
  \item{\dots}{name/value pairs to construct attributes in the 'open' tags 
  (ignored for the data.frame method)}
}
\details{
  The default method simply coerces its argument to character.
  
  The character method pastes \code{tag} as an XML element name (in brackets)
  before and after \code{x}.  Dots are used to construct attributes.  Like
  \code{tag} itself, the attributes can be vectors; in both cases the usual 
  recycling rules apply, as for \code{paste}.
  
  The data.frame method calls the character method on each of its columns, passing
  'keyname' and 'keys' as 'tag' and 'key', respectively.  'keys' should generally
  be unique.  Note that the columns have no parent: supply one manually with \code{nest},
  if necessary.
}
\value{
character
}
\references{ mifuns.googlecode.com }
\author{Tim Bergsma}
\seealso{\code{\link{read.param}}}
\examples{
nest(toXML(head(Theoph)),tag='frame')
\keyword{ manip }

