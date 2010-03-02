#This script runs NONMEM models for the phase1 data.

getwd()
library(MIfuns)
dir('/common/NONMEM')
command <- '/common/NONMEM/nm7_osx2/test/nm7_osx2.pl'
cat.cov='SEX'
cont.cov=c('HEIGHT','WEIGHT','AGE')
par.list=c('CL','Q','KA','V','V2','V3')
eta.list=paste('ETA',1:10,sep='')

#Try a 1 CMT model.
NONR(
     run=1001,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)     
#Reasonable diagnostic plots, but no covariance step.
#Try a 2 CMT model
NONR(
     run=1002,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)     
#Flatter residuals, but still no covariance step.
#Model over parameterized?  Drop Etas on V2 and Q.  Drop additive error.
NONR(
     run=1003,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)     
#minimization terminates when V3 goes negative.
#try bounding.
NONR(
     run=1004,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)
#V3 seems to approach a boundary.
#Tweak intial.
NONR(
     run=1005,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     cont.cov=cont.cov,
     cat.cov=cat.cov,
     par.list=par.list,
     eta.list=eta.list,
     plotfile='../nonmem/*/diagnostics.pdf',
     streams='../nonmem/ctl'
)     
#Covariance succeeds.  Accept 1005 as final model.

#Predictive check.
t <- metaSub(
     as.filename('../nonmem/ctl/1005.ctl'),
     names=1105,
     pattern=c(
         '\\$THETA[^$]+',
         '\\$OMEGA[^$]+',
         '\\$SIGMA[^$]+',
         '\\$EST[^$]+',
         '\\$COV',
         '\\$TABLE.*'
     ),
     replacement=c(
         '$MSFI=../1005/1005.msf\n',
         ';$OMEGA\n',
         ';$SIGMA\n',
         '$SIMULATION ONLYSIM (1968) SUBPROBLEMS=500\n',
         ';$COV',
         '$TABLE DV NOHEADER NOPRINT FILE=./*.tab FORWARD NOAPPEND\n'
    ),
    fixed=FALSE,
    out='../nonmem/ctl',
    suffix='.ctl'
 )

scavenge <- function(expr,lines){
  x <- lines[grep(expr,lines,ignore.case=TRUE,perl=TRUE)]
  if(length(x))return(x[[1]]) else return('')
}
NONR(
     run=1105,
     command=command,
     project='../nonmem',
     grid=TRUE,
     nice=TRUE,
     diag=FALSE,
     streams='../nonmem/ctl'
)     

phase1 <- read.csv('../data/derived/phase1.csv',na.strings='.')
head(phase1)
phase1 <- phase1[is.na(phase1$C),c('SUBJ','TIME','DV')]
nrow(phase1)
phase1 <- phase1[rep(1:nrow(phase1),500),]
nrow(phase1)
phase1$SIM <- rep(1:272,each=500)
head(phase1)
pred <- scan('../nonmem/1105/1105.tab')
nrow(phase1)
length(pred)
phase1$PRED <- pred
head(phase1)
phase1 <- phase1[!is.na(phase1$DV),]
head(phase1)
library(reshape)
phase1 <- melt(phase1,measure.var=c('DV','PRED'))
head(phase1)

#plotstuff

#bootstrap estimates of parameters.
getwd()
dir.create('../nonmem/1005.boot')
dir.create('../nonmem/1005.boot/data')
dir.create('../nonmem/1005.boot/ctl')
t <- metaSub(
     as.filename('../nonmem/ctl/1005.ctl'),
     names=1:500,
     pattern=c(
         '1005',
         '../../data/derived/phase1.csv',
         '$COV',
         '$TABLE'
     ),
     replacement=c(
         '*',
         '../data/*.csv',
         ';$COV',
         ';$TABLE'
    ),
    fixed=TRUE,
    out='../nonmem/1005.boot/ctl',
    suffix='.ctl'
 )
 bootset <- read.csv('../data/derived/phase1.csv')
 set.seed(1968)
 r <- resample(
 	bootset,
 	names=1:500,
 	key='ID',
 	rekey=TRUE,
 	out='../nonmem/1005.boot/data',
 	stratify='SEX'
 )

NONR(
     run=1:500,
     command=command,
     project='../nonmem/1005.boot/',
     boot=TRUE,
     nice=TRUE,
     diag=FALSE,
     streams='../nonmem/1005.boot/ctl'
)     
for(run in 1:1)runlog(
                      run,
                      outfile=file.path(
                        '../nonmem/1005.boot',
                        paste(
                              run,
                              'boot',
                              sep='.'
                        ),
                        paste(
                            run,
                            'lst',
                            sep='.'
                       )
                     )
)
