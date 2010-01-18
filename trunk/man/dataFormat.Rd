\name{dataFormat}
\alias{dataFormat}

\title{Integrate NONMEM Data for Plotting.}
\description{
 Integrates data.frames representing standard NONMEM output and the source dataset.
}
\usage{
dataFormat(
	tabfile, 
	covfile, 
	parfile, 
	dvname = "DV", 
	logtrans = FALSE, 
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99, 
	run, 
	...
)
}

\arguments{
  \item{tabfile}{data.frame for *.TAB}
  \item{covfile}{data.frame for underlying dataset,one record per ID}
  \item{parfile}{data.frame for *par.TAB}
  \item{dvname}{see PLOTR}
  \item{logtrans}{see PLOTR}
  \item{grp}{see PLOTR}
  \item{grpnames}{see PLOTR}
  \item{cont.cov}{see PLOTR}
  \item{cat.cov}{see PLOTR}
  \item{par.list}{see PLOTR}
  \item{eta.list}{see PLOTR}
  \item{missing}{see PLOTR}
  \item{run}{see PLOTR}
  \item{\dots}{passed to synthesis()}
}
\details{
  Called by dataSynthesis, and in turn calls synthesis().
}
\value{data.frame}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{dataSynthesis}}}
		\item{}{\code{\link{synthesis}}}
	}
}
\keyword{manip}

