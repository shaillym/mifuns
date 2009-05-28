\name{nm}
\alias{nm}
\alias{as.nm}
\alias{as.nm.data.frame}
\alias{merge.nm}
\alias{read.nm}
\alias{write.nm}


\title{Create and Manipulate NM Objects}
\description{
Objects of class \code{nm} are intended to support analysis using the software
NONMEM ((c), Icon Development Solutions). \code{nm()} gives a zero-row \code{data.frame} with
suitable columns and column classes (essentially, a template for dataset construction).  
\code{as.nm()} and \code{as.nm.data.frame} construct an
\code{nm} object from an existing object.  The read and write methods are wrappers
for \code{.csv} equivalents.  \code{merge.nm()} coerces its result using \code{as.nm()}, guaranteeing
a consistent left hand object when using \code{Ops.keyed}.
}
\usage{
nm()
%as.nm(x, ...)
\method{as.nm}{data.frame}(x, ...)
\method{merge}{nm}(x, y, ...)
read.nm(x)
write.nm(x, file, na = ".", row.names = FALSE, quote = FALSE, ...)
}
\arguments{
  \item{x}{\code{data.frame} or \code{nm}}
  \item{y}{right merge argument}
  \item{\dots}{extra arguments, ignored or passed to \code{write.csv}}
  \item{file}{passed to \code{write.csv}}
  \item{na}{passed to \code{write.csv}}
  \item{row.names}{passed to \code{write.csv}}
  \item{quote}{passed to \code{write.csv}} 
}

\details{
\code{as.nm.data.frame()} is the only method that creates an \code{nm} classification.  It 
alone enforces all qualities of class \code{nm}.

  \item \code{SUBJ} must be present and defined, even for commented records.
  \item \code{C} (class \code{comment}) will be created if not present.
  \item \code{NA} \code{C} will be imputed \code{FALSE}.
  \item Every active (non-commented) source record should define exactly one of \code{TIME} or \code{DATETIME}.
  \item \code{TIME} is received as-is, taken to represent relative accumulation of hours from arbitrary origin.
  \item \code{DATETIME} is understood as seconds, coercible to \code{miDateTime}, and trumps \code{TIME} with warning.
  \item If \code{DATETIME} is present, definition (or not) should be constant within subject (for active records).
  \item \code{SEQ} (class \code{flag}) will be created if not present.
  \item \code{nm} will be keyed on \code{SUBJ}, \code{TIME}, and \code{SEQ}. \code{SEQ} determines sort order for rows with matching \code{TIME}.
  \item Result will be sorted.
  \item \code{TIME} will be relativized to earliest extant value, incl. those in comments.
  \item \code{TAFD} (time after first dose), \code{TAD} (time since most recent dose), and \code{LDOS} (most recent dose) will be calculated if \code{AMT} is present.
  \item \code{TAD} will consider \code{ADDL} and \code{II} if present.
  \item \code{NA} flags will be imputed as zero.
  \item \code{MDV} (missing dependent value) will be calculated if \code{DV} is present.
  \item resulting column order will lead with \code{C} followed by key columns.
  
  Column summary:
  \item required inputs: SUBJ, TIME or DATETIME
  \item optional inputs: AMT, ADDL, II, DV, 
  \item enforced outputs: SUBJ, ID, C, TIME, SEQ
  \item conditional outputs: TAFD, TAD, LDOS, MDV

}
\value{
\code{write.nm} is used for side effects. Others return an object with class 
\code{c('nm','keyed','data.frame')}.
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\note{
In the examples below, note that assembly chains beginning with \code{nm()} will 
have class \code{nm} for every sub-result.  This requires more evaluations of \code{as.nm()}
but is useful if later operations depend on imputed or calculated variables.
Note, for example, that \code{-.moot()} only makes sense if the left operand already
resolves to class \code{nm}, since currently \code{moot.nm()} is the only method defined for
generic \code{moot()}.
}

\seealso{\code{\link{Ops.keyed}}}
\examples{
dose <- data.frame( 
	SUBJ = rep(letters[1:3], each = 2), 
	TIME = rep(c(0,20),3), 
	AMT = rep(c(40,60,80), each = 2) 
) 
dose <- as.keyed(dose,key=c('SUBJ','TIME'))
samp <- data.frame( 
	SUBJ = rep(letters[1:3], each = 4), 
	TIME = rep(c(0,10,20,30),3), 
	DV = signif(rnorm(12),2) + 2 
) 
samp <- as.keyed(samp,key=c('SUBJ','TIME'))
demo <- data.frame( 
	SUBJ = letters[2:5], 
	RACE = c('asian','white','black','other'), 
	SEX = c('female','male','female','male'), 
	WT = c(75, 70, 73, 68) 
)
demo <- as.keyed(demo,key=c('SUBJ'))
meds <- as.keyed(
	data.frame(
		SUBJ=c('a','c'),
		TIME=c(0,15),
		STOP=c(10,25),
		C3A4=as.flag(c(1,1))
	),
	key=c('SUBJ','TIME')
)

dose + samp
dose & samp
samp - dose
dose | demo
demo | dose

nm()
nm() + dose
as.nm(dose)
as.nm(dose + samp)
as.nm(dose + samp | demo) #as.nm executes once
nm() + dose + samp | demo #same result, but as.nm executes 4 times

meds
long <- deranged(meds,start='TIME',stop='STOP')
long$EVID <- 2
nm() + dose + samp + long
nm() + dose + samp + long - as.moot() 
nm() + dose + samp + as.rigged(n=10) 
data <- nm() + transform(dose,EVID=1) + transform(samp,EVID=0) | demo
summary(data,by=c('EVID','SEQ'))
}
\keyword{manip}

