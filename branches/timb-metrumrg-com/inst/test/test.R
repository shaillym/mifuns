library(MIfuns)
getwd()#...MIfunsTests
source('../MIfunsExamples/epilog.R')
metaSub(
        as.filename('MIfunsRunlogNM6.for'),
        names='MIfunsRunlogNM6',
        out='.',
        suffix='.for',
        pattern='SIZES',
        replacement='/common/NONMEM/nm6osxbeta/SIZES'
)
nonr <- function(
        NMcom='/common/NONMEM/nm6osxbeta/test/nm6osxbeta.pl',
	checkrunno=FALSE,
	dvname='Response',
	grp='SEX',
	grpnames=c('female','male'),
	cont.cov=c('AGE','HT','WT','CRCL','CLCR'),
	cat.cov='SEX',
	par.list=c('CL','V','V2','V3','KA','CLCM','CLCB','CLHC'),
	eta.list=paste('ETA',1:6,sep=''),
        ...
)NONR(
       NMcom=NMcom,
       checkrunno=checkrunno,
       dvname=dvname,
       grp=grp,
       grpnames=grpnames,
       cont.cov=cont.cov,
       cat.cov=cat.cov,
       par.list=par.list,
       eta.list=eta.list,
       ...
)
        
#nix workstation
nonr(b=1:2)
rlog(1:2,out='runlog.csv',append=FALSE)

#nix grid
bootdir <- paste(getwd(),'*.boot',sep='/')
rundir  <- paste(getwd(),'*',sep='/')
nms <- 1001:1005
nonr(b=nms,boot=FALSE,concurrent=FALSE,grid=FALSE,outdir=rundir )#conventional
nonr(b=nms,boot=FALSE,concurrent=FALSE,grid=TRUE ,outdir=rundir )#unnecessary chaining
nonr(b=nms,boot=FALSE,concurrent=TRUE ,grid=FALSE,outdir=rundir )#cross-chatter on stdout
nonr(b=nms,boot=FALSE,concurrent=TRUE ,grid=TRUE ,outdir=rundir )#conventional grid
nonr(b=nms,boot=TRUE ,concurrent=FALSE,grid=FALSE,outdir=bootdir)#boot-style directories
nonr(b=nms,boot=TRUE ,concurrent=FALSE,grid=TRUE ,outdir=bootdir)#chained boots, no plotting (outdir irrelevant)
nonr(b=nms,boot=TRUE ,concurrent=TRUE ,grid=FALSE,outdir=bootdir)#concurrent non-grid boots (chatter)
nonr(b=nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE ,outdir=bootdir)#conventional boots
nonr(b=nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE ,urgent=TRUE   )#urgent boots
rlog(nms,boot=TRUE,append=FALSE,out='bootlog.csv')
