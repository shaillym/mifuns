library(MIfuns)
getwd()#...MIfuns/inst/test/nm6/script
nonr <- function(
        run,
        command='/common/NONMEM/nm7_osx2/test/nm7_osx2.pl',
	dvname='Response',
	grp='SEX',
	grpnames=c('female','male'),
	cont.cov=c('AGE','HT','WT','CRCL','CLCR'),
	cat.cov='SEX',
	par.list=c('CL','V','V2','V3','KA','CLCM','CLCB','CLHC'),
	eta.list=paste('ETA',1:6,sep=''),
        nice=TRUE,
        project='../out',
        boot=FALSE,
        plotfile='../out/*/diagnostics.pdf',
        streams='../ctl',
        ep=runlog,
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
       nice=nice,
       project=project,
       boot=boot,
       plotfile=plotfile,
       streams=streams,
       epilog=ep,
       ...
)

#nix workstation
nonr(1)
PLOTR(1,project='../out',plotfile='../out/1/diagnostics.pdf')
nonr(1,split=TRUE)
nonr(1,execute=FALSE)
nonr(1,compile=FALSE)
nonr(1:2)
rlog(1:2,project='../out',out='../out/runlog.csv',append=FALSE)

#nix grid
nonr(1,grid=TRUE)
nonr(1,grid=TRUE,execute=FALSE)
nonr(1,grid=TRUE,compile=FALSE)
nms <- 1001:1003
nonr(nms,boot=FALSE,concurrent=FALSE,grid=FALSE)#conventional
nonr(nms,boot=FALSE,concurrent=FALSE,grid=TRUE )#unnecessary chaining
nonr(nms,boot=FALSE,concurrent=TRUE ,grid=FALSE)#cross-chatter on stdout
nonr(nms,boot=FALSE,concurrent=TRUE ,grid=TRUE )#conventional grid
nonr(nms,boot=TRUE ,concurrent=FALSE,grid=FALSE)#boot-style directories
nonr(nms,boot=TRUE ,concurrent=FALSE,grid=TRUE )#chained boots
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=FALSE)#concurrent non-grid boots (chatter)
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE )#conventional boots
nonr(nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE )#urgent boots
for(nm in nms)runlog(nm,outfile=file.path('../out',paste(nm,'boot',sep='.'),paste(nm,'lst',sep='.')))
rlog(nms,project='../out',boot=TRUE,append=FALSE,out='../out/bootlog.csv')

#windows
nonr(1)
nonr(1,invisible=TRUE)
