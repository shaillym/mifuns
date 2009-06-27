`PLOTR` <-
function (b, ProjectDir=getwd(), dvname = 'DV', logtrans = FALSE, 
    grp = NULL, grpnames = NULL, cont.cov = NULL, 
    cat.cov = NULL, par.list = NULL, eta.list = NULL, 
    missing = -99,epilog=NULL,file=NULL,...) 
{
    if(is.null(file))file <- paste(ProjectDir, '/', 'DiagnosticPlotReview', grp, '_',b, '.pdf', sep = '')
    missing <- as.numeric(as.character(missing))
    if (!is.null(epilog))try(source(epilog, local = TRUE, print.eval = TRUE))
    nonmdir <- filename(ProjectDir, b)
    tabfile <- filename(ProjectDir, b, '.TAB')
    outfile <- filename(nonmdir, b, '.lst')
    ctlfile <- filename(nonmdir, b, '.ctl')
    datfile <- getdname(ctlfile)
    if (!file.exists(outfile)) stop(paste(outfile,'does not exist.'))
    if (!file.exists(tabfile)) stop(paste(tabfile,'does not exist.'))
    tabfile <- getTabs(tabfile,b,ProjectDir)    
    covfile <- getCovs(datfile,nonmdir)
    covfile <- covfile[covfile$ID %in% tabfile$ID), ]#partial support for NONMEM 'ignore' ?moot due to stableMerge below
    grp <- limitGrp(grp,union(names(covfile),names(tabfile)))
    if (!is.null(grp)) tabfile <- stableMerge(tabfile, covfile[, c('ID', grp[!grp %in% names(tabfile)])])
    if (logtrans) tabfile <- transform(tabfile,DV =exp(DV),PRED=exp(PRED),IPRE=exp(IPRE))
    safe.call(pdf,file=file,...)
    lapply(diagnostics(grp, grpnames, ProjectDir, b, tabfile, dvname,...),print)
    lapply(doCov(b,ProjectDir,cont.cov,cat.cov,par.list,eta.list,covfile,tabfile,NULL,missing,...),print)
    dev.off()
    message(paste('Plotting for run ', b, ' complete.', sep = ''))
}

safe.call <- function(what,...){
	extras <- list(...)
	legal <- names(formals(what))
	extras <- extras[names(extras) %in% legal]
	do.call(what=what,args=extras)
}
filename <- function(dir,b=NULL,ext=NULL)paste(dir,'/',b,ext,sep='')
getCwres <- function(b,ProjectDir){
    	nonmdir <- filename(ProjectDir, b)
        cwrtab1 <- filename(nonmdir, NULL, '/cwtab1.deriv')
        if (!file.exists(cwrtab1)) message(paste(cwrtab1,'does not exist.'))
        if (!file.exists(cwrtab1)) return(NULL)
	tab.prefix <- filename(ProjectDir, b, '/cwtab')
	cwres.all<-compute.cwres(
		run.number=1,
		tab.prefix=tab.prefix,
		printToOutfile=TRUE
	)
	data.cwres <- read.table(
		file = filename(nonmdir,NULL, '/cwtab1'), 
		skip = 1, 
		header = TRUE
	)
        data.cwres$CWRES
}
setCwres <- function(b,ProjectDir,cwres=getCwres(b,ProjectDir)){
	if(!is.null(cwres)){
		write(
			paste(
				'Table output from NONMEM for Run ', 
				b, 
				' with CWRES as last column on', 
				format(
					Sys.time(), 
					'%a %b %d, %X, %Y'
				), 
				sep=''
			),
			file=filename(ProjectDir, b, '.TAB'),
			ncolumns=1
		)
		suppressWarnings(
			write.table(
				tabfile, 
				file = filename(ProjectDir, b, '.TAB'), 
				sep = ' ', 
				quote = FALSE, 
				row.names = FALSE, 
				col.names = TRUE, 
				append = TRUE, 
				na = '.'
			)
		)
	}
}       
getCovs <- function(file,dir){
	    here <- getwd()
	    setwd(dir)
	    covfile <- NULL
	    try(covfile <- read.table(file = file, header = TRUE, as.is = TRUE, sep = ','))
	    setwd(here)
	    if(is.null(covfile))return(covfile)
	    covfile <- covfile[covfile$C != 'C',]
	    covfile <- covfile[!duplicated(covfile$ID),]
	    return(covfile)
}
getPars <- function(file){ 
	    if (!file.exists(file))warning(paste(filename,'does not exist.'))
	    if (!file.exists(file))return(NULL)
	    file <- read.table(file = file, header = TRUE, skip = 1)
            file <- file[!duplicated(file$ID),]
	    return(file)
}  
limitGrp <- function(grp,options){
	if(is.null(grp))return(grp)
	notfound <- setdiff(grp,options)
        if (length(notfound)) warning(paste('ignoring grp variable',paste(notfound,collapse=', '),'.'))
        grp <- intersect(grp,options)
	if(!length(grp))return(NULL)
	grp
}
getdname <- function(filename){
	    datamod <- '^\\$DATA +([^ ]+).*$'
	    control <- scan(file = filename, what = '', comment.char = '', allowEscapes = TRUE, sep = '\n', quiet = TRUE)
	    dablock <- grep('\\$DATA', control, value = TRUE)
	    datfile <- sub(datamod, '\\1', dablock)
	    return(datfile)
}
getTabs <- function(file,b,ProjectDir){
	    tabfile <- read.table(file = file, header = TRUE, as.is = TRUE, skip = 1, comment.char = ''))
	    tabfile$ID <- as.character(tabfile$ID)
	    if(!'CWRES' %in% names(tabfile))tabfile$CWRES <- getCwres(b,ProjectDir)
	    if(!is.null(tabfile$CWRES))setCwres(b,ProjectDir,tabfile$CWRES)
	    tabfile <- tabfile[tabfile$EVID == 0, ]
	    tabfile
}    
    

