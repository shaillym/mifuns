`runNonmem` <-
function (
	run,
	command,
	ProjectDir,
	boot,
	urgent,
	checkrunno,
	diag,
	fdata,
	epilog,
	dvname,
	logtrans,
	grp,
	grpnames,
	cont.cov,
	cat.cov,
	par.list,
	eta.list,
	missing,
	invisible,
	checksum,
	grid,
	nice,
	udef,
	file,
	compile,
	execute,
	split,
	outdir = ProjectDir,
	runext = if(boot) '.boot' else if(grid) '.lock' else '',
	rundir = filename(ProjectDir,run,runext),
	outfile = filename(rundir,run,'.lst'),
	streams = ProjectDir,
	ctlfile = filename(streams,run,'.ctl'),
	remove  = c('^F[ISRC]','^OU','^nonmem.exe',if(fdata)c('^FD','^PR')),
	...
){
  #Define some functions.
  final <- function(x)sub('\\.lock','',x)
  
  #Groom arguments.
  outdir <- star(outdir,run)
  rundir <- star(rundir,run)
  ctlfile <- star(ctlfile,run)
  outfile <- star(outfile,run)
  tabfile <- tabfile(ctlfile,dir=final(rundir),...)
  parfile <- parfile(ctlfile,dir=final(rundir),...)
  msffile <- msffile(ctlfile,dir=final(rundir),...)
  if(!file.exists(ctlfile))message(paste(ctlfile,'was not found'))
  if(!file.exists(ctlfile))return()
  control <- explicitPath(readLines(ctlfile))
  if (checkrunno) writeLines(control <- fixFile(fixProblem(control,run),run),con=ctlfile)
  script <- NULL
  epimatch <- try(match.fun(epilog),silent=TRUE)
  if(is.function(epimatch))epilog <- epimatch
  else if (class(epilog)=='character'){
	  script <- epilog
	  epilog <- episcript
  }
  
  #Prepare the file environment.
  if(compile){
  	  if(!is.null(file))if(file.exists(file))file.remove(file)
	  if(file.exists(outfile))file.remove(outfile)
	  if(file.exists(tabfile))file.remove(tabfile)
	  if(file.exists(parfile))file.remove(parfile)
	  if(file.exists(msffile))file.remove(msffile)
	  purge.dir(final(rundir),nice)
	  if(rundir!=final(rundir))purge.dir(rundir)
	  dir.create(rundir, showWarnings = FALSE)
	  dname <- getdname(ctlfile)
	  if(!file.exists(resolve(dname,rundir)))warning(paste(dname,'not visible from',rundir))
	  if(!file.exists(resolve(dname,rundir)))return()
	  file.copy(ctlfile, file.path(rundir,basename(ctlfile)), overwrite = TRUE)
  }
  #Run NONMEM.
  runCommand(
  	command=command,
	run=run,
	rdir=rundir,
	boot=boot,
	urgent=urgent,
	checksum=checksum,
	grid=grid,
	udef=udef,
	ctlfile=file.path(rundir,basename(ctlfile)),
	outfile=outfile,
	invisible=invisible,
	compile=compile,
	execute=execute,
	split=split,
	...
  )

  #Clean up.
  if(execute){
	  if(boot & grid)return() #boot runs on grid have sync=n by definition, so run perh. not complete.
	  lapply(remove,purge.files,dir=rundir)
	  if(rundir!=final(rundir)){
		dir.create(final(rundir), showWarnings = FALSE)
		file.copy(from=dir(rundir,full.names=TRUE),to=final(rundir),overwrite=TRUE)
		purge.dir(rundir)
	  }
	
	  #Diagnostics
	  if(!udef)
	   if(nmVersion(config(dirname(command))) < 7)
	    try(setCwres(cwres=getCwres(directory=final(rundir)),file=tabfile))
	  if(diag)try(
		PLOTR(
			run,
			ProjectDir,
			dvname,
			logtrans,
			grp,
			grpnames,
			cont.cov,
			cat.cov,
			par.list,
			eta.list,
			missing,
			file=file,
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			outdir=outdir,
			...
		)
	  )
	  if (!is.null(epilog))if(is.function(epilog))try(
		  epilog(
			run=run,
			ProjectDir=ProjectDir,
			dvname=dvname,
			logtrans=logtrans,
			grp=grp,
			grpames=grpnames,
			cont.cov=cont.cov,
			cat.cov=cat.cov,
			par.list=par.list,
			eta.list=eta.list,
			missing=missing,
			ctlfile=ctlfile,
			outfile=final(outfile),
			rundir=final(rundir),
			outdir=outdir,
			...,
			script=script
		)
	  )
	
	  message(paste("Run ", run, " complete.", sep = ""))
  }
}

#.............................................................................
  purge.dir <- function(dir,nice=FALSE){
  	if(file_test('-d',dir)){
  		files <- dir(dir,full.names=TRUE,all.files=!nice)
  		files <- files[!files %in% grep('\\.$',files,value=TRUE)]
  		if(length(files))file.remove(files)
  		if(!nice)unlink(dir, recursive=TRUE)
  	}
  }
  purge.files <- function(pattern,dir='.'){
  	if(file_test('-d',dir)){
  		files <- dir(dir)
  		files <- grep(pattern,files,value=TRUE,ignore.case=TRUE)
  		if(length(files))file.remove(paste(dir,files,sep='/'))
  	}
  }
   episcript <- function(script,...){
	 extras <- list(...)
	 args <- names(extras)
	 lapply(
	 	args,
		function(x,extras)assign(x,extras[[x]]),
		extras
	)
	try(source(script))
  }
  fixProblem <- function(x,run)sub('(^\\$(PROB(LEM)? +(RUN#?)?([^ ]+)(.*$)',paste(sep='','\\1',run,'\\5'),x,ignore.case=TRUE)
  fixFile <- function(x,run){
        x <- explicitPath(x)
	risk <- grep('\\bTAB\\b|\\bMSF\\b',x,ignore.case=TRUE)
        except <- grep('\\bMSFI\\b',x,ignore.case=TRUE)
        risk <- setdiff(risk,except)
        dir <- dirname(x)
	base <- basename(x)
	base[risk] <- sub('^[^.(par)]+',run,base[risk])
	file.path(dir,base)
  }
  explicitPath <- function(x){
	risk <- grep('\\.TAB\\b|\\.MSF\\b',x,ignore.case=TRUE)
    	except <- grep('/',x)
    	risk <- setdiff(risk,except)
    	x[risk] <- sub('^(.*\\W)?(\\w*)(\\.msf|\\.tab)(.*)$','\\1./\\2\\3\\4',x[risk],ignore.case=TRUE)
	x
  }
  extractPath <- function(x)sub('(^.*(MSFO?|FILE) *= *)([^ ]*)(.*$)','\\3',x,ignore.case=TRUE)
  resolve <- function(file,dir)ifelse(contains('^\\.',file),file.path(dir,file),file)
  scavenge <- function(expr,lines)lines[grep(expr,lines,ignore.case=TRUE, perl=TRUE)][[1]]
  extfile <- function(ctlfile,dir,extreg,...)resolve(extractPath(scavenge(extreg,explicitPath(readLines(ctlfile)))),dir)  
  tabfile <- function(ctlfile,dir,tabreg='(?<!par)\\.tab',...)extfile(ctlfile,dir,extreg=tabreg,...)
  parfile <- function(ctlfile,dir,parreg='par\\.tab',...)extfile(ctlfile,dir,extreg=parreg,...)
  msffile <- function(ctlfile,dir,msfreg='^(?!\\$MSFI).*\\.msf',...)extfile(ctlfile,dir,extreg=msfreg,...)
	  
	  
	  
	  
	  
	  
	  
