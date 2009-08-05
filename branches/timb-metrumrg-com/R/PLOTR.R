`PLOTR` <-
function (
	b, 
	ProjectDir=getwd(), 
	dvname = 'DV', 
	logtrans = FALSE, 
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99,
	epilog=NULL,
	file=NULL,
	...
){

    #process data
    synthesis <- dataSynthesis(b,ProjectDir,dvname,logtrans,grp,grpnames,cont.cov,cat.cov,par.list,eta.list,missing,...)
    write.csv(synthesis,filename(ProjectDir,b,'_syn.csv'),row.names=FALSE)
    available <- names(synthesis)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    
    
    #open device
    if(is.null(file))file <- paste(ProjectDir,'/DiagnosticPlotReview',paste(grp,collapse=''),'_',b,'.pdf',sep = '')
    file <- sub('*', b, file,fixed=TRUE)
    safe.call(pdf,file=file,...)

    #make plots
    lapply(diagnosticPlots(synthesis, dvname=dvname, group='grpnames', model= paste('Model',b),...),print)
    lapply(covariatePlots(synthesis,cont.cov,cat.cov,par.list,eta.list,...),print)
    lapply(cwresPlots(synthesis,cont.cov,cat.cov,...),print)
    
    #cleanup
    dev.off()
    unlink(filename(ProjectDir,b,'_syn.csv'))
    message(paste('Plotting for run ', b, ' complete.', sep = ''))
    
    #try epilog
    if (!is.null(epilog))try(source(epilog, local = TRUE, print.eval = TRUE))#match NONR data environment

}

#filters elipses for functions that do not accept them
safe.call <- function(what,...){
	extras <- list(...)
	legal <- names(formals(what))
	extras <- extras[names(extras) %in% legal]
	do.call(what=what,args=extras)
}

#creates a filepath from dir,run, and extension
filename <- function(dir,b=NULL,ext=NULL)paste(dir,'/',b,ext,sep='')

#calculates a vector of cwres
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

#loads non-null cwres onto tab file
setCwres <- function(
	b=NULL,
	ProjectDir=NULL,
	cwres=NULL,
	file=NULL
){
	if((is.null(cwres)|is.null(file)) & (is.null(b)|is.null(ProjectDir)))stop('Specify crwes and file, or b and ProjectDir.')
	if(is.null(cwres))cwres=getCwres(b,ProjectDir)
	if(is.null(cwres))return(NULL)
	if(is.null(file))file=filename(ProjectDir, b, '.TAB')
	cwres <- c('CWRES',cwres)
	tabfile <- readLines(file)[-1]
	if(length(tabfile)!=length(cwres))stop()
	msg <- paste('CWRES added', format(Sys.time(), '%a %b %d, %X, %Y'))
	tabfile <- paste(tabfile,cwres)
	tabfile <- c(msg,tabfile)
	writeLines(tabfile,con = file)
}


#finds the nonmem data set and coverts it to a covariate file
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

#returns the parameter file
getPars <- function(file){ 
	    if (!file.exists(file))warning(paste(filename,'does not exist.'))
	    if (!file.exists(file))return(NULL)
	    file <- read.table(file = file, header = TRUE, skip = 1)
            file <- file[!duplicated(file$ID),]
	    return(file)
}  

#scavenges the data set name/path from the control stream
getdname <- function(filename){
	    datamod <- '^\\$DATA +([^ ]+).*$'
	    control <- scan(file = filename, what = '', comment.char = '', allowEscapes = TRUE, sep = '\n', quiet = TRUE)
	    dablock <- grep('\\$DATA', control, value = TRUE)
	    datfile <- sub(datamod, '\\1', dablock)
	    return(datfile)
}

#finds the tab file and reduces it to observation rows
getTabs <- function(file=filename(ProjectDir, b, '.TAB'),b,ProjectDir){
	    tabfile <- read.table(file = file, header = TRUE, as.is = TRUE, skip = 1, comment.char = '')
	    #tabfile$ID <- as.character(tabfile$ID)
	    tabfile <- tabfile[tabfile$EVID == 0, ]
	    tabfile
}   

#melds the grpnames columns into one, renaming levels conditionally
groupnames <- function(data,grp,grpnames=NULL,b){
	    result <- factor(
	    	do.call(
  			paste,
				c(
					as.list(data[,grp,drop=FALSE]),
					sep=", "
				)
			)
		)
	   nlevs <- length(levels(result))
	   if(!is.null(grpnames))if(length(grpnames)==nlevs)levels(result) <- grpnames
	   if(!is.null(grpnames))if(length(grpnames)!=nlevs)warning(
		   paste(
  			"Run", 
			b, 
			"has",
			nlevs,
			"grouping levels but",
			length(grpnames),
			"grpnames (ignored)." 
		)
	  )
	  result
}

#combines any number of tables using left join to load the columns specified in x
synthesis <- function(x,key=character(0),frames,...){
    if(any(sapply(frames,function(x)!inherits(x,'data.frame'))))stop()    
    x <- unique(x)
    y <- frames[[1]]
    frames[[1]] <- NULL
    x <- setdiff(x,names(y))
    while (length(x) & length(frames)){
	    z <- frames[[1]]
	    frames[[1]] <- NULL
	    z <- z[,union(key,intersect(x,names(z)))]
	    z <- z[!duplicated(z[,intersect(names(z),names(y))]),]
	    y <- stableMerge(y,z)
	    x <- setdiff(x,names(y))
    }
    y
}

#reduces a character vector to the subset found in options
strain <- function(x,options){
	    if(!is.null(x))x <- intersect(x,options)
            x
}

#back transforms cols in x
backtrans <- function(x,cols){
	    for(col in cols)x[[col]] <- exp(x[[col]])
	    x
}

#generates the plotting data set, given actual data frames, etc.
dataFormat <- function(
	tabfile,
	covfile,
	parfile,
	dvname='DV',
	logtrans=FALSE,
	grp=NULL,
	grpnames=NULL,
	cont.cov=NULL,
	cat.cov=NULL,
	par.list=NULL,
	eta.list=NULL,
	missing=-99,
	b,
	...
){
    if (logtrans) tabfile <- backtrans(tabfile,c(dvname,'PRED','IPRE'))    
    available <- unique(c(names(tabfile),names(covfile),names(parfile)))
    grp <- strain(grp,available)
    cont.cov <- strain(cont.cov,available)
    cat.cov <- strain(cat.cov,available)
    par.list <- strain(par.list,available)
    eta.list <- strain(eta.list,available)
    requests <- c(grp,cont.cov,cat.cov,par.list,eta.list)
    synthesis <- synthesis(requests, key='ID',frames=list(tabfile,covfile,parfile),...)
    missing <- as.numeric(as.character(missing))
    for(col in cont.cov) synthesis[[col]] <- as.numeric(as.character(synthesis[[col]]))
    for(col in cont.cov) synthesis[[col]][!is.na(synthesis[[col]]) & synthesis[[col]]==missing] <- NA
    if(is.null(grp))synthesis$grpnames <- 'all'
    if(is.null(grp))grp <- 'grpnames'
    synthesis$grpnames <- groupnames(synthesis,grp,grpnames,b)
    synthesis
}

#generates the plotting data set, given ProjectDir, b, etc.
dataSynthesis <-
function (
	b, 
	ProjectDir=getwd(), 
	dvname = 'DV', 
	logtrans = FALSE, 
	grp = NULL, 
	grpnames = NULL, 
	cont.cov = NULL, 
	cat.cov = NULL, 
	par.list = NULL, 
	eta.list = NULL, 
	missing = -99,
	...
){
    #dereference directory context
    nonmdir <- filename(ProjectDir, b)
    tabfile <- filename(ProjectDir, b, '.TAB')
    outfile <- filename(nonmdir, b, '.lst')
    ctlfile <- filename(nonmdir, b, '.ctl')
    datfile <- getdname(ctlfile)
    parfile <- filename(ProjectDir, b, 'par.TAB')
    if (!file.exists(outfile)) stop(paste(outfile,'does not exist.'))
    if (!file.exists(tabfile)) stop(paste(tabfile,'does not exist.'))
    if (!file.exists(parfile)) stop(paste(parfile,'does not exist.'))

    #acquire data
    tabfile <- getTabs(tabfile,b,ProjectDir)    
    covfile <- getCovs(datfile,nonmdir)
    parfile <- getPars(parfile)
    
    #process data
    synthesis <- dataFormat(tabfile,covfile,parfile,dvname,logtrans,grp,grpnames,cont.cov,cat.cov,par.list,eta.list,missing,b,...)
    synthesis
}
   

