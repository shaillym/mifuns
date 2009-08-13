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
      if (nix()) if(!boot) system(paste('rm -rf ', rdir, '/FD*', sep = '')) 
      if (nix()) if( boot) if (!file.exists(paste(rdir, '/FILE10', sep = ''))) {
          system(paste('rm -rf ', rdir, '/F*', sep = ''))
          system(paste('rm -rf ', rdir, '/nonmem.exe', sep = ''))
          system(paste('rm -rf ', rdir, '/P*', sep = ''))
          system(paste('rm -rf ', rdir, '/O*', sep = ''))
          system(paste('rm -rf ', rdir, '/Run*', sep = ''))
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

