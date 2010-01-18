\name{plotFileName}
\alias{plotFileName}

\title{Make a Name for a Diagnostic Plot File}
\description{
 Make a name for a diagnostic plot file, given supplied arguments.
}
\usage{
plotFileName(
	run,
	dir=getwd(),
	grp=NULL,
	stem='DiagnosticPlotReview',
	pext='.pdf',
	...
)
}

\arguments{
  \item{run}{run name}
  \item{dir}{directory name}
  \item{grp}{filename modifiers, see NONR}
  \item{stem}{filename stem}
  \item{pext}{extension, typ. pdf}
  \item{\dots}{ignored}
 }
\value{character}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{PLOTR}}}
	}
}
\keyword{manip}

