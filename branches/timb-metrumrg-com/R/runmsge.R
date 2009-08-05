`runmsge` <-
function (
	NMcom, ProjectDir, b, boot, concurrent, Platform, SGEflgs, 
    checkrunno, diag, fdata, epilog, dvname, logtrans, 
     grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
    missing, dosbox, nochecksum, grid, nice, udef, file,...
){

  #Set NONMEM output directory.
  #origin <- getwd()
  ndir <- filename(ProjectDir, b)
  bootstrap <- boot %in% c(1,3)
  
  #Set runtime directory.
  if (concurrent & !bootstrap) rdir <- filename(ProjectDir, b, '.lock')
  if (concurrent & bootstrap) rdir <- filename(ProjectDir, b, '.boot')
  if (grid==FALSE) rdir <- ndir
  
  #Name both copies of the control stream.
  ctl1 <- filename(ProjectDir, b, '.ctl')
  ctl2 <- filename(      rdir, b, '.ctl')
  
  #Groom the control stream.
  if (checkrunno) {
    a <- scan(file = ctl1, what = '', comment.char = '', allowEscapes = TRUE, sep = '\n', quiet = TRUE)
    a <- sub('#.[0-9]{1,4}', paste('# ', b, sep = ''), a)
    a <- gsub('E=../[0-9]{1,4}', paste('E=../', b, sep = ''), a)
    a <- gsub('O=../[0-9]{1,4}', paste('O=../', b, sep = ''), a)
    write.table(a, file = ctl1, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
    
  #Prepare the file environment.
  #setwd(ProjectDir) 
  purge.files(paste('^',b,'[^0-9]*\\.TAB$',sep=''),ProjectDir)
  purge.files(paste('^[^0-9]*',b,'\\.PDF$',sep=''),ProjectDir)
  purge.dir(ndir,nice)
  if(contains('\\.lock$',rdir))purge.dir(rdir)
  dir.create(rdir, showWarnings = FALSE)
  file.copy(ctl1, ctl2, overwrite = TRUE)
  #setwd(rdir)
  
  #Run NONMEM.
  runnm(NMcom, ProjectDir, b, boot, concurrent, Platform, SGEflgs, dosbox, nochecksum, grid,udef,...)
  
  #Clean up (if not bootstrap run).
  if(!bootstrap){
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
  		#setwd(ndir)
  		purge.dir(rdir)
  	}
  	Sys.chmod(dir(ndir), mode='0664')
  	#setwd('..')
  	Sys.chmod(dir(ProjectDir,'\\.TAB$',full.names=TRUE), mode='0664')
  	Sys.chmod(dir(ProjectDir,'\\.MSF$',full.names=TRUE), mode='0664')
  }
  
  #Diagnostics
  if(!bootstrap)try(setCwres(b, ProjectDir))
  if(diag & !bootstrap)try(
    	PLOTR(
    		b, ProjectDir, dvname, logtrans, 
            grp, grpnames, cont.cov, cat.cov, par.list, eta.list, 
            missing, file=file,...
        )
    )
  #setwd(origin)
  if (!is.null(epilog))try(source(epilog, local = TRUE, print.eval = TRUE))
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

