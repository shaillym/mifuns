library(MIfuns)
getwd()#...MIfuns/inst/test/nm6/script
source('../../../example/epilog.R')
nonr <- function(
        run,
        command='/common/NONMEM/nm6/test/nm6.pl',
	dvname='Response',
	grp='SEX',
	grpnames=c('female','male'),
	cont.cov=c('AGE','HT','WT','CRCL','CLCR'),
	cat.cov='SEX',
	par.list=c('CL','V','V2','V3','KA','CLCM','CLCB','CLHC'),
	eta.list=paste('ETA',1:6,sep=''),
        nice=TRUE,
        ProjectDir='../out',
        outdir='../out/*',
        streams='../ctl',
        ep=epilog,
        ...
)NONR(
       run=run,
       command=command,
       dvname=dvname,
       grp=grp,
       grpnames=grpnames,
       cont.cov=cont.cov,
       cat.cov=cat.cov,
       par.list=par.list,
       eta.list=eta.list,
       epilog=ep,
       nice=nice,
       ProjectDir=ProjectDir,
       outdir=outdir,
       streams=streams,
       ...
)

#nix workstation
nonr(1)
PLOTR(1,ProjectDir='../out',outdir='../out/1')
nonr(1,split=TRUE)
nonr(1,execute=FALSE)
nonr(1,compile=FALSE)
nonr(1:2)
rlog(1:2,ProjectDir='../out',out='../out/runlog.csv',append=FALSE)

#nix grid
nonr(1,grid=TRUE)
nonr(1,grid=TRUE,execute=FALSE)
nonr(1,grid=TRUE,compile=FALSE)
bootdir <- paste(getwd(),'../out/*.boot',sep='/')
rundir  <- paste(getwd(),'../out/*',sep='/')
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
rlog(nms,ProjectDir='../out',boot=TRUE,append=FALSE,out='../out/bootlog.csv')

#windows
nonr(1)
nonr(1,invisible=TRUE)
