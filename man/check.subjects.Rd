\name{check.subjects}
\alias{check.subjects}
\title{Summarize Columns in a Data.frame}
\description{
  This function checks for missing data, numeric data, ranges, etc.
  It is a tool developed to review data that has been read into R.
}
\usage{
check.subjects(IDname, data)
}
\arguments{
  \item{IDname}{subject identifier in the data}
  \item{data}{data.frame}
}
\value{
  A summary of each column in the \code{data} by \code{IDname} is output.
 }
\references{mifuns.googlecode.com}
\author{Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others).}
\keyword{manip}
