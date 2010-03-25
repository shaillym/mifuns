\name{panel.stratify}
\alias{panel.stratify}
\title{Handle Each Level of a Stripplot Separately}
\description{
  Just as panel.superpose handles each group of data separately, panel.stratify
  handles each 'level' of data separately.  Typically, levels are the unique
  values of y (horizontal==TRUE) that result from a call to stripplot or bwplot.
  The default panel functions treat all levels simultaneously.  Plotting some 
  transformation of the data (e.g. density polygons for each level) is much easier if 
  the levels are presented individually.
}
\usage{
panel.stratify(
	x, 
	y, 
	type = "p", 
	groups = NULL, 
	pch = if (is.null(groups)) 
	plot.symbol$pch else superpose.symbol$pch, 
	col, 
	col.line = if (is.null(groups)) 
	plot.line$col else superpose.line$col, 
	col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
	font = if (is.null(groups)) 
	plot.symbol$font else superpose.symbol$font, 
	fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
	fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
	lty = if (is.null(groups)) 
	plot.line$lty else superpose.line$lty, 
	cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
	fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
	lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
	horizontal = FALSE, 
	panel.levels = "panel.xyplot", 
	..., 
	jitter.x = FALSE, 
	jitter.y = FALSE, 
	factor = 0.5, 
	amount = NULL
)
}
\arguments{
  \item{x}{See panel.xyplot}
  \item{y}{See panel.xyplot}
  \item{type}{See panel.xyplot}
  \item{groups}{See panel.xyplot}
  \item{pch}{See panel.xyplot}
  \item{col}{See panel.xyplot}
  \item{col.line}{See panel.xyplot}
  \item{col.symbol}{See panel.xyplot}
  \item{font}{See panel.xyplot}
  \item{fontfamily}{See panel.xyplot}
  \item{fontface}{See panel.xyplot}
  \item{lty}{See panel.xyplot}
  \item{cex}{See panel.xyplot}
  \item{fill}{See panel.xyplot}
  \item{lwd}{See panel.xyplot}
  \item{horizontal}{See panel.xyplot}
  \item{panel.levels}{a function to handle each unique level of the data}
  \item{\dots}{See panel.xyplot}
  \item{jitter.x}{See panel.xyplot}
  \item{jitter.y}{See panel.xyplot}
  \item{factor}{See panel.xyplot}
  \item{amount}{See panel.xyplot}
}
\details{
  panel.stratify is defined almost identically to panel.xyplot.  panel.levels
  is analogous to panel.groups.  panel.levels may want to handle special cases
  of col, which may be missing if groups is NULL and may be NA if groups is 
  not NULL (set to NA by panel.superpose).
  
  x and y are split into subsets by whichever of them represents levels (y if 
  horizontal is TRUE, x otherwise).  Corresponding subsets of x and y are 
  forwarded one at a time, along with the other arguments, to panel.levels.  
  Additionally, the current value of 'level' as well as the complete vector of 
  'levels' are available to panel.levels.
}
\value{used for side effects}
\references{mifuns.googlecode.com}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{panel.covplot}}}
		\item{}{\code{\link{panel.densitystrip}}}
		\item{}{\code{\link{panel.xyplot}}}
	}
}
\keyword{manip}

