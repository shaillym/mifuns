\name{miTran-package}
\alias{miTran-package}
\alias{miTran}
\docType{package}
\title{
Assemble nm datasets systematically.
}
\description{
NMTRAN(c) is an interface to NONMEM(c) that requires datasets with regular
properties.  Generating valid NMTRAN datasets can be tedious.  miTran
represents a system for assembling NMTRAN datasets efficiently.
}
\details{
\tabular{ll}{
Package: \tab miTran\cr
Type: \tab Package\cr
Version: \tab 3.10\cr
Date: \tab 2009-05-26\cr
License: \tab GPL-2\cr
Requires: \tab MIfuns; grid\cr
}

This version anticipates merge with MIfuns.
\itemize{
\item \code{\link{as.comment}}
\item \code{\link{contains}}
\item \code{\link{distance}}
\item \code{\link{deranged}}
\item \code{\link{as.flag}}
\item \code{\link{as.keyed}}
\item \code{\link{nm}}
\item \code{\link{nmPlots}}
\item \code{\link{Ops.keyed}}
\item \code{\link{plot.nm}}
\item \code{\link{reapply}}
\item \code{\link{summary.nm}}
\item \code{\link{text2decimal}}
}
}
\author{
Tim Bergsma
Maintainer: Tim Bergsma <timb@metrumrg.com>
}
\references{
Beal SL, Sheiner LB. NONMEM Users Guide - Part I-VIII.  1988. University of 
California at San Francisco, San Francisco. 
}
\keyword{package}
\keyword{manip}
\seealso{
itemize{
\item \code{\link[MIfuns:MIfuns-package]{MIfuns}}
}
}
