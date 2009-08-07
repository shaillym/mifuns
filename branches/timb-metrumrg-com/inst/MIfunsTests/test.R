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
	...
){
	InstDir <- absdir(InstDir)
	NonmemDir <- absdir(NonmemDir)
	ProjectDir <- absdir(ProjectDir)
	twig <- twig(NonmemDir)
	if(is.null(NMcom)) NMcom <- paste(NonmemDir,'/test/',twig,'.pl',sep='') 
	infn <- paste(InstDir,'MIfunsExamples/MIfunsRunlogNM6.for',sep='/')
	misc <- paste(ProjectDir,'0',sep='/')
	dir.create(misc)
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
		checkrunno=FALSE,
		dvname='Response',
		grp='SEX',
		grpnames=c('female','male'),
		cont.cov=c('AGE','HT','WT','CRCL','CLCR'),
		cat.cov='SEX',
		par.list=c('CL','V','V2','V3','KA','CLCM','CLCB','CLHC'),
		eta.list=paste('ETA',1:6,sep=''),
		file=paste(ProjectDir,'*','diagnostics.pdf',sep='/'),
		tabfile=paste(ProjectDir,'*','*.TAB',sep='/'),
		parfile=paste(ProjectDir,'*','*par.TAB',sep='/')
	)		
}

getwd()#...MIfunsTests

test(InstDir='..',NonmemDir='~/NONMEM/nm6osx1',b=1:2)
rlog(1:2,out='0/runlog.csv',append=FALSE)
