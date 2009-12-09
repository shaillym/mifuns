`runNonmem` <-
function (
	command, 
	ProjectDir, 
	run, 
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
	ctlfile = NULL,
	outfile = NULL,
	tabfile = NULL,
	msffile = NULL,
	parfile = NULL,
	outdir  = ProjectDir,
	pattern = c(
		'^F[ISRC]',
		'^OU',
		'^nonmem.exe',
		if(fdata)c('^FD','^PR')
	),
	...
){
  #Define some functions.
  final <- function(x)sub('\\.lock','',x)
  runtime <- function(x,rundir){
	  file <- rev(strsplit(x,'/')[[1]])[[1]]
	  paste(rundir,file,sep='/')
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
	
  #Groom arguments
  outdir <- star(outdir,run)
  script <- NULL
  epimatch <- try(match.fun(epilog),silent=TRUE)
  if(is.function(epimatch))epilog <- epimatch
  else if (class(epilog)=='character'){
	  script <- epilog
	  epilog <- episcript
  }
	  
  #Set runtime directory.
  rundir <- filename(ProjectDir,run)
  if (grid) rundir <- filename(ProjectDir, run, '.lock')
  if (boot) rundir <- filename(ProjectDir, run, '.boot')
  
  #Check arguments.
  if(is.null(ctlfile)) ctlfile <- filename(ProjectDir, run, '.ctl')
  ctlfile <- star(ctlfile,run)
  if(is.null(outfile)) outfile <- filename(rundir,run,'.lst')
  if(is.null(tabfile)) tabfile <- filename(outdir, run, '.TAB')
  if(is.null(parfile)) parfile <- filename(outdir, run, 'par.TAB')
  if(is.null(msffile)) msffile <- filename(outdir, run, '.MSF')
  outfile <- star(outfile,run)
  tabfile <- star(tabfile,run)
  parfile <- star(parfile,run)
  msffile <- star(msffile,run)
  
  #Groom the control stream.
  if (checkrunno) {
    txt <- readLines(ctlfile)
    txt <- sub('#.[0-9]{1,4}', paste('# ', run, sep = ''), txt)
    txt <- gsub('E=../[0-9]{1,4}', paste('E=../', run, sep = ''), txt)
    txt <- gsub('O=../[0-9]{1,4}', paste('O=../', run, sep = ''), txt)
    writeLines(txt,ctlfile)
  }
    
  #Prepare the file environment.
  #purge.files(paste('^',run,'[^0-9]*\\.TAB$',sep=''),ProjectDir)
  #purge.files(paste('^[^0-9]*',run,'[^.]*\\.pdf$',sep=''),ProjectDir)
  if(!is.null(file))if(file.exists(file))file.remove(file)
  if(file.exists(outfile))file.remove(outfile)
  if(file.exists(tabfile))file.remove(tabfile)
  if(file.exists(parfile))file.remove(parfile)
  if(file.exists(msffile))file.remove(msffile)
  purge.dir(final(rundir),nice)
  if(grid)purge.dir(rundir)
  dir.create(rundir, showWarnings = FALSE)
  file.copy(ctlfile, runtime(ctlfile,rundir), overwrite = TRUE)
  
  #Run NONMEM.
  runCommand(
  	command=command,
	rdir=rundir,
	run=run,
	boot=boot,
	urgent=urgent,
	checksum=checksum,
	grid=grid,
	udef=udef,
	ctlfile=runtime(ctlfile,rundir),
	outfile=outfile,
	invisible=invisible,
	...
  )
  
  #Clean up.
  if(boot & grid)return() #boot runs on grid have sync=n by definition, so run perh. not complete.
  lapply(pattern,purge.files,rundir)
  Sys.chmod(dir(rundir,paste('^',run,'\\.',sep=''),full.names=TRUE),mode='0664')
  Sys.chmod(dir(rundir,'n.*\\.',full.names=TRUE),mode='0664')
  runfilename <- function(rundir)dir(rundir,'^Run',full.names=TRUE)
  try(file.rename(runfilename(rundir),sub('\\.o[0-9]*$','.out',runfilename(rundir))),silent=TRUE)
  if(grid){ 
  	dir.create(final(rundir), showWarnings = FALSE)
  	file.copy(from=dir(rundir,full.names=TRUE),to=final(rundir),overwrite=TRUE)
   	purge.dir(rundir)
  }
  Sys.chmod(dir(final(rundir)), mode='0664')
  Sys.chmod(tabfile, mode='0664')
  Sys.chmod(parfile, mode='0664')
  Sys.chmod(msffile, mode='0664')
  
  #Diagnostics
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
		tabfile=tabfile,
		ctlfile=ctlfile,
		parfile=parfile,
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
		tabfile=tabfile,
		ctlfile=ctlfile,
		parfile=parfile,
		outfile=final(outfile),
		rundir=final(rundir),
		outdir=outdir,
		...,
		script=script
	)
  )

  message(paste("Run ", run, " complete.", sep = ""))
}
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
 
