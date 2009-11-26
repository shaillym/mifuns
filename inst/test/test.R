library(MIfuns)
getwd()#...MIfuns/inst/test
source('../example/epilog.R')
metaSub(
        as.filename('MIfunsRunlogNM6.for'),
        names='MIfunsRunlogNM6',
        out='.',
        suffix='.for',
        pattern='SIZES',
        replacement='/common/NONMEM/nm6osx6/SIZES'
)
nonr <- function(
        b,
        NMcom='/common/NONMEM/nm6osx6/test/nm6osx6.pl',
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
       b=b,
       NMcom=NMcom,
       checkrunno=checkrunno,
       dvname=dvname,
       grp=grp,
       grpnames=grpnames,
       cont.cov=cont.cov,
       cat.cov=cat.cov,
       par.list=par.list,
       eta.list=eta.list,
       epilog=epilog,
       ...
)

#nix workstation
nonr(1:2)
rlog(1:2,out='runlog.csv',append=FALSE)
nonr(1,split=TRUE)


#nix grid
nonr(1,split=TRUE,grid=TRUE)
bootdir <- paste(getwd(),'*.boot',sep='/')
rundir  <- paste(getwd(),'*',sep='/')
nms <- 1001:1005
nonr(nms,boot=FALSE,concurrent=FALSE,grid=FALSE,outdir=rundir )#conventional
nonr(nms,boot=FALSE,concurrent=FALSE,grid=TRUE ,outdir=rundir )#unnecessary chaining
nonr(nms,boot=FALSE,concurrent=TRUE ,grid=FALSE,outdir=rundir )#cross-chatter on stdout
nonr(nms,boot=FALSE,concurrent=TRUE ,grid=TRUE ,outdir=rundir )#conventional grid
nonr(nms,boot=TRUE ,concurrent=FALSE,grid=FALSE,outdir=bootdir)#boot-style directories
nonr(nms,boot=TRUE ,concurrent=FALSE,grid=TRUE ,outdir=bootdir)#chained boots, no plotting (outdir irrelevant)
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=FALSE,outdir=bootdir)#concurrent non-grid boots (chatter)
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE ,outdir=bootdir)#conventional boots
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE ,urgent=TRUE   )#urgent boots
rlog(nms,boot=TRUE,append=FALSE,out='bootlog.csv')

#windows
nonr(1)
nonr(1,invisible=TRUE)

