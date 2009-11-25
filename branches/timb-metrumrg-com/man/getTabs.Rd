\name{getTabs}
\alias{getTabs}

\title{Return the Observation Rows (EVID==0) Corresponding to the Specified Filename}
\description{
 Useful for returning the PK portion of NONMEM run output.
}
\usage{
getTabs(file)
}

\arguments{
  \item{file}{path for a file, typ. *.TAB}
}
\details{
  
}
\value{data.frame}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}

\seealso{ \code{\link{PLOTR}} }
\examples{





function(file){
	    tabfile <- read.table(file = file, header = TRUE, as.is = TRUE, skip = 1, comment.char = '')
	    #tabfile$ID <- as.character(tabfile$ID)
	    tabfile <- tabfile[tabfile$EVID == 0, ]
	    tabfile
  }
}


\keyword{manip}

