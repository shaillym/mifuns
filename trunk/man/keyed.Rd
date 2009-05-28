\name{as.keyed}
\alias{[.keyed}
\alias{aggregate.keyed}
\alias{as.keyed.data.frame}
\alias{as.keyed}
\alias{constant.keyed}
\alias{dupKeys}
\alias{key}
\alias{key<-}
\alias{merge.keyed}
\alias{naKeys}
\alias{print.keyed.summary}
\alias{sort.keyed}
\alias{summary.keyed}
\alias{transform.keyed}
\alias{uniKey.keyed}
\alias{uniKey}



\title{Create and Manipulate Keyed Data Frames}
\description{The class \code{keyed} is a subclass of data.frame with a \code{key} attribute.
The key is a vector of column names which, taken together, should provide enough
information to uniquely distinguish each row.  Specific functions and methods
take advantage of this information.
}
\usage{
\method{[}{keyed}(x, ...)
\method{aggregate}{keyed}(x, by=x[,setdiff(key(x),across),drop=FALSE], FUN, across=character(0), ...)
%as.keyed(x, ...)
\method{as.keyed}{data.frame}(x, key, ...)
\method{constant}{keyed}(x, ...)
dupKeys(x, ...)
key(x, ...)
key(x) <- value
\method{merge}{keyed}(x, y, ...)
naKeys(x, ...)
\method{print}{keyed.summary}(x, ...)
\method{sort}{keyed}(x, decreasing = FALSE, ...)
\method{summary}{keyed}(object, ...)
\method{transform}{keyed}(`_data`, ...)
%uniKey(x, ...)
\method{uniKey}{keyed}(x,key=NULL,...)
}
\arguments{
  \item{x}{a (\code{keyed}) \code{data.frame}}
  \item{by}{a list of indicies as long as \code{nrow(x)}, whose interaction gives the aggregates (groups)}
  \item{FUN}{an aggregating function}
  \item{across}{column names in \code{key(x)} across which to aggregate; see details}
  \item{\dots}{extra arguments, usually ignored, but passed to FUN in \code{aggregate}}
  \item{key}{a \code{character} vector of column names in \code{x} that should uniquely distinguish each row}
  \item{value}{a key (\code{character} vector of column names)}
  \item{y}{the right argument in the merge}
  \item{decreasing}{(coercible to) \code{logical}; length 1 or length \code{key(x)}}
  \item{object}{a \code{keyed data.frame}}
  \item{_data}{a \code{keyed data.frame}}


  
}
\details{

The generic \code{as.keyed()} is the usual way of creating a keyed object.  The method
\code{as.keyed.data.frame()} calls \code{key<-()}. The function \code{key()} allows checking an 
object's key. A \code{data.frame} can be re-keyed by a subsequent call.  Generally, a 
\code{data.frame} should be keyed on columns that actually exist, but this is not
enforced.

In \code{aggregate.keyed()}, the default behavior is to aggregate by the key columns, i.e.,
to eliminate duplicate keys by aggregation.  \code{by} can be specified arbitrarily, but 
must be a named list (e.g., a \code{data.frame}) with each element as long as \code{nrow(x)}.  
Each element in \code{by} will displace any like-named element in \code{x}, and 
\code{names(by)} will  serve as the key of the result.  If \code{by} has length zero,
(as it does by default when \code{across} is \code{key(x)}) the entire data set is aggregated
into a one row \code{data.frame}.

\code{across} is a convenience argument to \code{aggregate.keyed()}.  If specified, it must be
a subset of (or all of) \code{key(x)}.  Columns indicated by \code{across} are dropped 
from \code{x} and from the default \code{by} value, and aggregation proceeds irrespective
of those columns.

The method \code{constant.keyed()} returns unique rows for columns that are constant 
within the key.

The function \code{naKeys()} detects rows for which one or more key fields is \code{NA}.

The function \code{dupKeys()} detects all rows for which there is another row 
(earlier or later) with identical key. That means it can never identify a single
row, as \code{duplicated()} can: it identifies the duplicates as well as those rows of 
which they are duplicates. It is recommended to test for \code{NA}s before testing 
for duplicates.

\code{order()} treats "decreasing" as an all-or-nothing argument, whereas \code{sort.keyed()} 
allows independent specification for each column.

Methods for \code{merge()} and \code{transform()} are key-friendly. The method for \code{summary()}
is key-centric.  \code{uniKey()} creates a single character vector from all the key columns.
}
\value{
Most functions and methods documented here return objects with class 
\code{c('keyed','data.frame')}.

\code{key<-()} and methods for \code{summary()} and \code{print()} are used for side effects.

\code{unikey.keyed()} returns a \code{character} vector as long as \code{nrow(x)}.

\code{naKeys()} and \code{dupKeys()} return \code{logical} vectors as long as \code{nrow(x)}.

}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{\code{\link{Ops.keyed}}}
\examples{
a <- sort(as.keyed(Theoph,key=c('Subject','Time')))
summary(a)
aggregate(a, across='Time',FUN=mean)
}
\keyword{manip}

