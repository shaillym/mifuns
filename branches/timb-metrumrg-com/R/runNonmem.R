`runNonmem` <-
function (
	NMcom, 
	ProjectDir, 
	b, 
	boot, 
	SGEflgs, 
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
	dosbox, 
	nochecksum, 
	grid, 
	nice, 
	udef, 
	file, 
	ctlfile = NULL,
	outfile = NULL,
	tabfile = NULL,
	msffile = NULL,
	parfile = NULL,
	...
){
  #Set NONMEM output directory.
  bootstrap <- boot %in% c(1,3)
  ndir <- filename(ProjectDir, b)
  rdir <- ndir
  
  #Set runtime directory.
  if (grid & !bootstrap) rdir <- filename(ProjectDir, b, '.lock')
  if (grid & bootstrap)  rdir <- filename(ProjectDir, b, '.boot')
  
  #Check arguments.
  if(is.null(ctlfile)) ctlfile <- filename(ProjectDir, b, '.ctl')
  ctlfile <- star(ctlfile,b)
  newfile <- rev(strsplit(ctlfile,'/')[[1]])[[1]]
  newfile <- paste(rdir,newfile,sep='/')
  if(is.null(outfile)) outfile <- sub('\\.ctl$','.lst',newfile)
  if(is.null(tabfile)) tabfile <- filename(ProjectDir, b, '.TAB')
  if(is.null(parfile)) parfile <- filename(ProjectDir, b, 'par.TAB')
  if(is.null(msffile)) msffile <- filename(ProjectDir, b, '.MSF')
  outfile <- star(outfile,b)
  tabfile <- star(tabfile,b)
  parfile <- star(parfile,b)
  msffile <- star(msffile,b)
  
  #Groom the control stream.
  if (checkrunno) {
    txt <- readLines(ctlfile)
    txt <- sub('#.[0-9]{1,4}', paste('# ', b, sep = ''), txt)
    txt <- gsub('E=../[0-9]{1,4}', paste('E=../', b, sep = ''), txt)
    txt <- gsub('O=../[0-9]{1,4}', paste('O=../', b, sep = ''), txt)
    writeLines(txt,ctlfile)
  }
    
  #Prepare the file environment.
  purge.files(paste('^',b,'[^0-9]*\\.TAB$',sep=''),ProjectDir)
  purge.files(paste('^[^0-9]*',b,'\\.PDF$',sep=''),ProjectDir)
  if(file.exists(outfile))file.remove(outfile)
  if(file.exists(tabfile))file.remove(tabfile)
  if(file.exists(parfile))file.remove(parfile)
  if(file.exists(msffile))file.remove(msffile)
  purge.dir(ndir,nice)
  if(contains('\\.lock$',rdir))purge.dir(rdir)
  dir.create(rdir, showWarnings = FALSE)
  file.copy(ctlfile, newfile, overwrite = TRUE)
  ctlfile <- newfile
  
  #Run NONMEM.
  runCommand(
  	NMcom, 
	ProjectDir, 
	b, 
	boot, 
	SGEflgs, 
	dosbox, 
	nochecksum, 
	grid, 
	udef, 
	ctlfile, 
	outfile,
	...
  )
  
  #Clean up (if not bootstrap run).
  if(bootstrap)return()
  purge.files('^F[ISRC].*',rdir)
  purge.files('^OU.*',rdir)
  purge.files('nonmem.exe',rdir)
  if(!fdata)purge.files('^FD.*',rdir)
  if(!fdata)purge.files('^PR.*',rdir)
  Sys.chmod(dir(rdir,paste('^',b,'\\.',sep=''),full.names=TRUE),mode='0664')
  Sys.chmod(dir(rdir,'n.*\\.',full.names=TRUE),mode='0664')
  try(file.rename(dir(rdir,'^Run',full.names=TRUE),'nonmem.log'),silent=TRUE)
  if(contains('\\.lock$',rdir)){ 
  	dir.create(ndir, showWarnings = FALSE)
  	file.copy(from=dir(rdir,full.names=TRUE),to=ndir,overwrite=TRUE)
   	purge.dir(rdir)
  }
  Sys.chmod(dir(ndir), mode='0664')
  Sys.chmod(tabfile, mode='0664')
  Sys.chmod(parfile, mode='0664')
  Sys.chmod(msffile, mode='0664')
  
  #Diagnostics
  try(setCwres(cwres=getCwres(b,ProjectDir),file=tabfile))
  if(diag)try(
    	PLOTR(
    		b, 
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
		...
        )
  )
  if (!is.null(epilog))try(match.fun(epilog)(
        b=b,
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
	...
    )
  )
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

  
