library(MIfuns)

absdir <- function(x){
	startdir <- getwd()
	result <- try(setwd(x))
	if(inherits(result, 'try-error'))stop(paste(x,'not found'))
	result <- getwd()
	setwd(startdir)
	result
}
twig <- function(x){
	splits <- strsplit(x,'/')[[1]]
	len <- length(splits)
	splits[len]
}
test <- function(
	InstDir,
	NonmemDir,
	ProjectDir=getwd(),
	grid=FALSE,
	NMcom=NULL,
	b,
        outdir=paste(ProjectDir,'*',sep='/'),
	...
){
	InstDir <- absdir(InstDir)
	NonmemDir <- absdir(NonmemDir)
	ProjectDir <- absdir(ProjectDir)
	twig <- twig(NonmemDir)
        #if(is.null(rundir))rundir <- paste(ProjectDir,'*',sep='/')
	if(is.null(NMcom)) NMcom <- paste(NonmemDir,'/test/',twig,'.pl',sep='') 
	infn <- paste(InstDir,'MIfunsExamples/MIfunsRunlogNM6.for',sep='/')
	misc <- paste(ProjectDir,'0',sep='/')
	if(!file.exists(misc))dir.create(misc)
	file.copy(infn,misc)
	infn <- paste(misc,'MIfunsRunlogNM6.for',sep='/')
	infText <- readLines(infn)
	infText <- sub('/common/NONMEM/nm6osx1',NonmemDir,infText)
	writeLines(infText,infn)
	source(paste(InstDir,'MIfunsExamples/epilogEx.R',sep='/'))
	NONR(
		NMcom=NMcom,
		b=b,
		ProjectDir=ProjectDir,
		epilog=epilog,
                grid=grid,
		checkrunno=FALSE,
		dvname='Response',
		grp='SEX',
		grpnames=c('female','male'),
		cont.cov=c('AGE','HT','WT','CRCL','CLCR'),
		cat.cov='SEX',
		par.list=c('CL','V','V2','V3','KA','CLCM','CLCB','CLHC'),
		eta.list=paste('ETA',1:6,sep=''),
		#file=paste(rundir,'diagnostics.pdf',sep='/'),
		#tabfile=paste(rundir,'*.TAB',sep='/'),
		#parfile=paste(rundir,'*par.TAB',sep='/'),
                outdir=outdir,
                ...
	)		
}

getwd()#...MIfunsTests

#nix workstation
test(InstDir='..',NonmemDir='~/NONMEM/nm6osx1',b=1:2)
rlog(1:2,out='0/runlog.csv',append=FALSE)

#nix grid
InstDir='..'
NonmemDir='/common/NONMEM/nm6osxbeta'
test(InstDir,NonmemDir,b=1,grid=FALSE)
test(InstDir,NonmemDir,b=1:2,grid=FALSE)
test(InstDir,NonmemDir,b=1,grid=TRUE)
test(InstDir,NonmemDir,b=1:2,grid=TRUE)


data <- read.csv('1.csv',strip.white=TRUE,as.is=TRUE)
nms <- 1001:1005

t <- metaSub(
    as.filename('1.ctl'),
    names=nms,
    pattern=c(
        'RUN# 1',
        '../1.csv',
        '1.TAB',
        '1par.TAB',
        '1.MSF'
    ),
    replacement=c(
        'RUN# *',
        '../*.csv',
        '*.TAB',
        '*par.TAB',
        '*.MSF'
    ),
    out=getwd(),
    suffix='.ctl',
    fixed=TRUE
)
r <- resample(
    data,
    names=nms,
    key='ID',
    rekey=TRUE,
    out=getwd()
)
NONR(NMcom='nm6osxbeta.pl',nms,boot=TRUE)
NONR(NMcom='nm6osxbeta.pl',nms,boot=TRUE,urgent=TRUE)
bootdir <- paste(getwd(),'*.boot',sep='/')
test(InstDir,NonmemDir,b=nms,boot=FALSE,concurrent=FALSE,grid=FALSE)#conventional
test(InstDir,NonmemDir,b=nms,boot=FALSE,concurrent=FALSE,grid=TRUE )#unnecessary chaining
test(InstDir,NonmemDir,b=nms,boot=FALSE,concurrent=TRUE ,grid=FALSE)#cross-chatter on stdout
test(InstDir,NonmemDir,b=nms,boot=FALSE,concurrent=TRUE ,grid=TRUE )#conventional grid
test(InstDir,NonmemDir,b=nms,boot=TRUE ,concurrent=FALSE,grid=FALSE,outdir=bootdir)#boot-style directories
test(InstDir,NonmemDir,b=nms,boot=TRUE ,concurrent=FALSE,grid=TRUE )#chained boots, no plotting (outdir irrelevant)
test(InstDir,NonmemDir,b=nms,boot=TRUE ,concurrent=TRUE ,grid=FALSE,outdir=bootdir)#concurrent non-grid boots (chatter)
test(InstDir,NonmemDir,b=nms,boot=TRUE ,concurrent=TRUE ,grid=TRUE )#conventional boots
system('qstat')
rlog(nms,boot=TRUE,append=FALSE,out='boots.log')
