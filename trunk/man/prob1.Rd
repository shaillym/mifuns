\name{prob1}
\alias{prob1}
\alias{ctl1}
\alias{infnf}
\alias{infnf6}
\docType{data}
\title{Example data for NONR}
\description{
 prob1 is a data frame for use with the control stream ctl1.  infnf and infnf6 are 
 INFN subroutines for use with ctl1 under NONMEM5 or NONMEM6, respectively.  After 
 loading the data (see usage), save objects as files, and edit paths as necessary.

 writeLines(ctl1,"1.ctl")
 writeLines(infnf,"runlog.for")
 writeLines(infnf6,"runlog.for")
 write.csv(prob1,"prob1.csv",row.names=FALSE)
 
}
\usage{
	data(ctl1)
	data(prob1)
	data(infnf)
	data(infn6)
}
\format{
  prob1 is tabular; the others are rows of text.
}
\keyword{datasets}
