\name{purge.files}
\alias{purge.files}

\title{Purge Files}
\description{
 If dir is a directory, remove files matching the pattern.
}
\usage{
purge.files(pattern, dir = ".")
}

\arguments{
  \item{pattern}{a regular expression}
  \item{dir}{a directory path}
}
\value{used for side effects.}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{help}} }
\examples{





function(pattern,dir='.'){
  	if(file_test('-d',dir)){
  		files <- dir(dir)
  		files <- grep(pattern,files,value=TRUE,ignore.case=TRUE)
  		if(length(files))file.remove(paste(dir,files,sep='/'))
  	}
  }
}


\keyword{manip}

