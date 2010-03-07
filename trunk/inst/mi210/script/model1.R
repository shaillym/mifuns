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
#Slightly better CWRES, but still no covariance step.
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
records <- nrow(phase1)
records
phase1 <- phase1[rep(1:records,500),]
nrow(phase1)
phase1$SIM <- rep(1:500,each=records)
head(phase1,300)
with(phase1,DV[SIM==1 & SUBJ==12])
with(phase1,DV[SIM==2 & SUBJ==12])
pred <- scan('../nonmem/1105/1105.tab')
nrow(phase1)
length(pred)
phase1$PRED <- pred
head(phase1)
phase1 <- phase1[!is.na(phase1$DV),]
head(phase1)

#plots
#Quick look at pred
library(lattice)
xyplot(
	PRED~DV,
	phase1,
	panel=function(...){
		panel.xyplot(...)
		panel.abline(a=0,b=1)
	}
)
xyplot(
	log(PRED)~log(DV),
	phase1,
	panel=function(...){
		panel.xyplot(...)
		panel.abline(a=0,b=1)
	}
)

#Since subjects may contribute differing numbers of observations, it may
#be useful to look at predictions from a subject-centric perspective.
#Therefore, we wish to calculate summary statistics for each subject, 
#(observed and predicted) and then make obspred comparisons therewith.
library(reshape)
subject <- melt(phase1,measure.var=c('DV','PRED'))
head(subject)
#We are going to aggregate each subject's DV and PRED values using cast().
#cast() likes an aggregation function that returns a list.
#We write one that grabs min med max for each subject, sim, and variable.
metrics <- function(x)list(min=min(x), med=median(x), max=max(x))
#Now we cast, ignoring time.
subject <- data.frame(cast(subject, SUBJ + SIM + variable ~ .,fun=metrics))
head(subject)
#Note that regardless of SIM, DV (observed) is constant.
#Now we can repeat earlier plots using aggregated data.  We need DV and PRED
#in separate columns, with min/med/max as the variable.
dvpred <- melt(subject,measure.var=c('min','med','max'),variable_name='metric')
head(dvpred)
dvpred <- data.frame(cast(dvpred, SUBJ + SIM + metric ~ variable))
head(dvpred)
#Now we can do seperate-axis comparisons of DV and PRED.
xyplot(
	log(PRED)~log(DV),
	dvpred,
	groups=metric,
	auto.key=TRUE,
	panel=function(...){
		panel.xyplot(...)
		panel.abline(a=0,b=1)
	}
)
#Now, our predictions have central tendencies, which can vary by SIM.
#Thus, our metrics as well can have variable central tendencies that vary by SIM.
#We want to represent the variability across SIMS by aggregating within SIM.
#That means aggregating across subjects, within SIMS.  
#There are many aggregation strategies, but we choose quantiles for a non-parametric 
#result. Quantiles that 'clip' the tails of the distribution offer robustness against
#number of SIMS.  Within each SIM, let's find for each metric the 5th, 50th, and 95th percentile.
#We also want to do this for the original data set (requires some minor rearrangement).
head(dvpred)
quants <- melt(dvpred,measure.var=c('DV','PRED'))
head(quants)
quants <- data.frame(cast(quants,SIM + metric + variable ~ .,fun=quantile,probs=c(0.05,0.50,0.95)))
head(quants,10)
#Note, again, that DV quantiles are invariant across SIMS.
#We now have a lot of display options.  The simplest is to plot DV~PRED for each quantile and metric.
#Requires slight rearrangement.
molten <- melt(quants, measure.var=c('X5.','X50.','X95.'),variable_name='quant')
head(molten)
frozen <- data.frame(cast(molten, SIM + metric + quant ~ variable))
head(frozen)
xyplot(
	log(PRED)~log(DV)|metric,
	frozen,
	groups=quant,
	layout=c(1,3),
	auto.key=TRUE,
	panel=function(...){
		panel.xyplot(...)
		panel.abline(a=0,b=1)
	}
)
#For a better view of the distributions, however, we can work with single-axis plot functions,
# using the molten data.
head(molten)
#haystack plot
stripplot(
	~value|metric+quant,
	molten,
	groups=variable,
	horizontal=TRUE,
	auto.key=TRUE,
	panel=panel.superpose,
	alpha=0.5,
	panel.groups=function(x,type,group.number,col.line,fill,col,...){
		#browser()
		view <- viewport(yscale=c(0,max(hist(x,plot=FALSE)$density)))
		pushViewport(view)
		if(group.number==1) panel.abline(v=x,col=col.line)
		else panel.histogram(x,breaks=NULL,col=fill,border=col.line,...)
		popViewport()
	}
)
#boa plot
stripplot(
	~value|metric+quant,
	molten,
	groups=variable,
	horizontal=TRUE,
	auto.key=TRUE,
	panel=panel.superpose,
	alpha=0.5,
	panel.groups = function(x,y,group.number,col,col.line,fill,font,...){
		if(group.number==1)panel.segments(x0=x,x1=x,y0=y,y1=max(current.panel.limits()$y),col=col.line,...)
		else panel.densitystrip(x=x,y=y,col=fill,border=col.line,...)
	}
)
#rearranged
stripplot(
	quant~value|metric,
	molten,
	groups=variable,
	horizontal=TRUE,
	auto.key=TRUE,
	panel=panel.superpose,
	alpha=0.5,
	layout=c(1,3),
	#scales=list(relation='free'),
	panel.groups = function(x,y,group.number,col,col.line,fill,font,...){
		if(group.number==1)panel.segments(x0=x,x1=x,y0=y,y1=y+1,col=col.line,...)
		else panel.densitystrip(x=x,y=y,col=fill,border=col.line,...)
	}
)
#reversed
stripplot(
	metric~value|quant,
	molten,
	groups=variable,
	horizontal=TRUE,
	auto.key=TRUE,
	panel=panel.superpose,
	alpha=0.5,
	layout=c(1,3),
	scales=list(relation='free'),
	panel.groups = function(x,y,group.number,col,col.line,fill,font,...){
		if(group.number==1)panel.segments(x0=x,x1=x,y0=y,y1=y+1,col=col.line,...)
		else panel.densitystrip(x=x,y=y,col=fill,border=col.line,...)
	}
)

#bootstrap estimates of parameters.
getwd()
dir.create('../nonmem/1005.boot')
dir.create('../nonmem/1005.boot/data')
dir.create('../nonmem/1005.boot/ctl')
t <- metaSub(
     as.filename('../nonmem/ctl/1005.ctl'),
     names=1:5,
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
     concurrent=FALSE,
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
