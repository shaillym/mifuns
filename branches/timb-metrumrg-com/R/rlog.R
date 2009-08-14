`rlog` <-
  function (
  	b, 
	boot=FALSE, 
	ProjectDir=getwd(), 
	append=TRUE,
	file=NULL,
	out=NULL,
	...
){
  #if(win()) boot <- FALSE
  if(is.null(out)) out <- paste(ProjectDir,'CombRunLog.csv',sep='/')
  if(!append) if(file.exists(out)) file.remove(out)
  for(i in b){
      #identify objects
      rdir <- paste(ProjectDir, '/', i, sep = '')    
      if(boot)rdir <- paste(ProjectDir, '/', i, '.boot', sep = '')
      if(is.null(file)) f <- paste(rdir,'NonmemRunLog.csv',sep='/')
      f <- star(f,i)
      #cleanup
      if (nix()) if(!boot) purge.files('/FD*',rdir)
      if (nix()) if( boot) if (!file.exists(paste(rdir, '/FILE10', sep = ''))) {
          purge.files('/F*',rdir)
          purge.files('/nonmem.exe',rdir)
          purge.files('/P*', rdir)
          purge.files('/O*',rdir)
          purge.files('/Run*',rdir)
      } 
      #append log
      if(!file.exists(f)) cat(paste('Log for Run ', i, ' does not exist', '\n', sep = ''))
      else {
	  dat <- readLines(f)
	  dat <- dat[nchar(dat)>0]
          cat(dat,sep='\n',file=out,append=TRUE)
      }
  }
 }

