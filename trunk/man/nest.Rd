\name{nest}
\alias{nest}
\title{Nest an XML Fragment in a Parent Element}
\description{
  Create matching open and close tags and embed an XML fragement between them.
}
\usage{
nest(x, tag, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an XML fragment}
  \item{tag}{an element name}
  \item{\dots}{ignored}
}
\details{
  Attributes are not supported.
}
\value{
  character
}
\references{ mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{\code{\link{bracket}},\code{\link{toXML.data.frame}},\code{\link{read.param}}}
\examples{
nest('some xml',tag='content')
}
\keyword{ manip }

