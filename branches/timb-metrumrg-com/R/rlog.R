`rlog` <-
  function (
  	b, 
	boot=0, 
	ProjectDir=getwd(), 
	append=TRUE,
	file=NULL,
	out=NULL,
	...
){
  boot <- as.integer(boot)
  boot <- boot %in% c(1,3)
  if(nix()) boot <- FALSE
  if(is.null(out)) out <- paste(ProjectDir,'CombRunLog.csv',sep='/')
  if(!append) if(file.exists(out)) file.remove(out)
  for(i in b){
      #identify objects
      rdir <- paste(ProjectDir, '/', i, sep = '')    
      if(boot)rdir <- paste(ProjectDir, '/', i, '.boot', sep = '')
      if(is.null(file)) file <- paste(rdir,'NonmemRunLog.csv',sep='/')
      file <- star(file,i)
      #cleanup
      if (nix) if(!boot) system(paste('rm -rf ', rdir, '/FD*', sep = '')) 
      if (nix) if( boot) if (!file.exists(paste(rdir, '/FILE10', sep = ''))) {
          system(paste('rm -rf ', rdir, '/F*', sep = ''))
          system(paste('rm -rf ', rdir, '/nonmem.exe', sep = ''))
          system(paste('rm -rf ', rdir, '/P*', sep = ''))
          system(paste('rm -rf ', rdir, '/O*', sep = ''))
          system(paste('rm -rf ', rdir, '/Run*', sep = ''))
      } 
      #append log
      if(!file.exists(file)) cat(paste('Log for Run ', i, ' does not exist', '\n', sep = ''))
      else cat(paste(readLines(file),'\n',sep=''),file=out,append=TRUE)
  }
  }

