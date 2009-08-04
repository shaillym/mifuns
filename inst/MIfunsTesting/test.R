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
	splits <- split(x,'/')
	len <- length(splits)
	splits[len]
}
battery <- function(
	InstDir,
	NonmemDir,
	ProjectDir=getwd(),
	grid=FALSE,
	NMcom=NULL,
	...
){
	InstDir <- absdir(InstDir)
	NonmemDir <- absdir(NonmemDir)
	ProjectDir <- absdir(Projectdir)
	twig <- twig(NonmemDir)
	if(is.null(NMcom)) NMcom <- paste(NonmemDir,'test',twig,'.pl') 
	infn <- paste(InstDir,'MIfunsExScripts/MIfunsRunlogNM6.for',sep='/')
	file.copy(infn,ProjectDir)
	infn <- paste(ProjectDir,'MIfunsExScripts/MIfunsRunlogNM6.for',sep='/')
	infText <- readLines(infn)
	infText <- sub('/common/NONMEM/nm6osx1',NonmemDir,infText)
	writeLines(infText,infn)
	epilog <- paste(InstDir,'MIfunsExScripts/epilogEx.R',sep='/')
	file.copy(epilog,ProjectDir)
	NONR(
		NMcom=NMcom,
		b=1:2,
		ProjectDir=ProjectDir,
		epilog=paste(ProjectDir,'epilogEx.R',sep='/'),
		dvname='Response',
		grp='SEX',
		grpnames=c('female','male'),
		cont.cov=c('AGE','HT','WT','CRCL'),
		cat.cov='SEX',
		par.list=c('CL','V','V2','KA'),
		eta.list=paste('ETA',1:5,sep='')
	)		
}







