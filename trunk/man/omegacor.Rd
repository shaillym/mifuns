\name{omegacor}
\alias{omegacor}
\title{
Convert NONMEM Omega Covariance Matrix to Correlation Matrix
}
\description{
This function operates on a character vector read from a NONMEM output (.lst)
file.  It finds the block of text representing final parameter estimates for 
Omega.  It harvests these and converts from covariance matrix to correlation 
matrix.
}
\usage{
omegacor(x, ...)
}

\arguments{
  \item{x}{
character vector representing NONMEM output file (*.lst)
}
  \item{\dots}{
other arguments, unused
}
}
\value{
correlation matrix
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{cov2cor}}}
	}
}
\example{
lst <- c(
'THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********',
'',
'',
'         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7     ',
' ',
'         8.58E+00  2.16E+01  6.84E-02  3.78E+00  1.07E+02  9.99E-01  1.67E+00',
'',
'',
'',
' OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********',
'',
'',
'            ETA1      ETA2      ETA3',
' ',
' ETA1',
'+        1.96E-01',
' ',
' ETA2',
'+        0.00E+00  1.29E-01',
' ',
' ETA3',
'+        0.00E+00  0.00E+00  1.07E-01',
' ',
'',
'',
' SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****',
'',
'',
'            EPS1',
' ',
' EPS1',
'+        6.71E-02'
) 
omegacor(lst)
}
\keyword{manip}

