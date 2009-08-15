library(MIfuns)
getwd()#...MIfunsTests
source('../MIfunsExamples/epilog.R')
metaSub(
        as.filename('0/MIfunsRunlogNM6.for'),
        names='MIfunsRunlogNM6',
        out='0',
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
rlog(1:2,out='0/runlog.csv',append=FALSE)

#nix grid
bootdir <- paste(getwd(),'*.boot',sep='/')
nms <- 1001:1005
nonr(b=nms,boot=FALSE,concurrent=FALSE,grid=FALSE)#conventional
nonr(b=nms,boot=FALSE,concurrent=FALSE,grid=TRUE )#unnecessary chaining
nonr(b=nms,boot=FALSE,concurrent=TRUE ,grid=FALSE)#cross-chatter on stdout
nonr(b=nms,boot=FALSE,concurrent=TRUE ,grid=TRUE )#conventional grid
nonr(b=nms,boot=TRUE ,concurrent=FALSE,grid=FALSE,outdir=bootdir)#boot-style directories
nonr(b=nms,boot=TRUE ,concurrent=FALSE,grid=TRUE )#chained boots, no plotting (outdir irrelevant)
nonr(b=nms,boot=TRUE ,concurrent=TRUE ,grid=FALSE,outdir=bootdir)#concurrent non-grid boots (chatter)
nonr(b=nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE )#conventional boots
rlog(nms,boot=TRUE,append=FALSE,out='0/boots.log')
