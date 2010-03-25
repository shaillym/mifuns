\name{packageCheck}
\alias{packageCheck}
\title{Load a package and run package-level examples for testing purposes.}
\description{
  packageCheck attempts to load the specified package and to run example() with
  the package name as an argument.
}
\usage{
packageCheck(x,lib.loc=NULL)
}
\arguments{
  \item{x}{atomic character, the name of a single package}
  \item{lib.loc}{the library to check, passed to library()}
}
\details{
 Many packages do not have package-level examples; the call to example() in 
 such cases does nothing (warnings are suppressed).
}
\value{
  an atomic character string: zero if the package does not load or if example() 
  generates an error; otherwise, the package version.
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{accept}}}
		\item{}{\code{\link{acceptance}}}
		\item{}{\code{\link{library}}}
	}
}
\keyword{ manip }

