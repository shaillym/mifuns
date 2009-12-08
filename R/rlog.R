`rlog` <-function(
  	run, 
	boot=FALSE, 
	ProjectDir=getwd(), 
	append=TRUE,
	file=NULL,
	out=filename(ProjectDir,'CombRunLog.csv'),
	pattern=if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	...
){
  if(!append) if(file.exists(out)) file.remove(out)
  run <- unique(run)
  for(each in run){
      #identify objects
      rdir <- paste(ProjectDir, '/', each, sep = '')    
      if(boot)rdir <- paste(ProjectDir, '/', each, '.boot', sep = '')
      if(is.null(file)) f <- paste(rdir,'NonmemRunLog.csv',sep='/')
      f <- star(f,each)
      #cleanup
      lapply(pattern,purge.files,rdir)
      #append log
      if(!file.exists(f)) cat(paste('Log for Run ', each, ' does not exist', '\n', sep = ''))
      else {
	  dat <- readLines(f)
	  dat <- dat[nchar(dat)>0]
          cat(dat,sep='\n',file=out,append=TRUE)
      }
  }
 }

