\name{as.nmcontrol}
\alias{as.nmcontrol}
\alias{as.character.nmcontrol}
\alias{as.list.nmcontrol}
\alias{as.nmcontrol.character}
\alias{format.nmcontrol}
\alias{print.nmcontrol}
\alias{read.nmcontrol}
\alias{write.nmcontrol}

\title{
Create, Manipulate, Read, and Write NONMEM Control Streams
}
\description{
This family of functions constitutes the class \code{nmcontrol}: an object
model of the NONMEM control stream.  \code{nmcontrol} models a control stream
as a list of records; each record is a character vector. The read and write 
functions (not generic) convert \code{nmcontrol} to and from file format.  The 
print, format, and \code{as.character} methods display \code{nmcontrol} as it normally looks
in a text editor.  \code{as.list.nmcontrol} simply unclasses its argument.
\code{as.nmcontrol.character} does the heavy work, breaking up a character
vector into records and storing as a list.
}
\usage{
\method{as.character}{nmcontrol}(x, ...)
\method{as.list}{nmcontrol}(x, ...)
\method{as.nmcontrol}{character}(
	x, 
	pattern = "^[[:blank:]]*\\$([^ ]+).*", 
	name = "\\1", 
	...
)
\method{format}{nmcontrol}(x, ...)
\method{print}{nmcontrol}(x, ...)
read.nmcontrol(con, ...)
write.nmcontrol(
	x, 
	file = "data", 
	ncolumns = 1, 
	append = FALSE, 
	sep = " ", 
	...
)
}
\arguments{
  \item{x}{an nmcontrol object (or analogous character vector)}
  \item{\dots}{extra arguments passed to other functions}
  \item{pattern}{regular expression for first line of a control record}
  \item{name}{regular expression (relative to \code{pattern} giving the name of the control record}
  \item{con}{a connection or the name of a file to open}
  \item{file}{passed to \code{write}}
  \item{ncolumns}{passed to \code{write}}
  \item{append}{passed to \code{write}}
  \item{sep}{passed to \code{write}}
}
\details{
Conveniently, the record indicator in NONMEM control stream syntax is the 
same as the element selector in R list syntax: \$.  The convention is that names
of elements in \code{nmcontrol} (lower case) are converted to record types (upper case)
in the control stream.  The user is free to add, delete, rearrange, and edit
records using standard list manipulation techniques. When printed, records 
appear in list order.
}
\value{
\item{as.character.nmcontrol }{a character vector representing a control stream}
\item{as.list }{a list representing a control stream}
\item{as.nmcontrol.character }{a control stream object}
\item{format.nmcontrol }{character}
\item{print.nmcontrol }{character}
\item{read.nmcontrol}{a control stream object}
\item{write.nmcontrol }{used for side effects}
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{write}}}
	}
}
\examples{
}
\keyword{manip}

