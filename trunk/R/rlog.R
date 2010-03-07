`rlog` <-function(
  	run, 
	boot=FALSE, 
	project=getwd(), 
	append=TRUE,
	file=NULL,
	out=filename(project,'CombRunLog.csv'),
	pattern=if(boot)c('^F','^nonmem.exe','^P','^O','^Run') else '^FD',
	software='nm6',
	...
){
  if(!append) if(file.exists(out)) file.remove(out)
  run <- unique(run)
  for(each in run){
      #identify objects
      rundir <- filename(project, each, if(boot) '.boot' else '')
      outfile = filename(rundir,run,'.lst')
      if(is.null(file)) f <- file.path(rundir,'NonmemRunLog.csv')
      f <- star(f,each)
      #cleanup
      lapply(pattern,purge.files,rundir)
      #append log
      if(software=='nm7')runlog(run=run,outfile=outfile,...)
      if(!file.exists(f)) cat(paste('Log for Run ', each, ' does not exist', '\n', sep = ''))
      else {
	  dat <- readLines(f)
	  dat <- dat[nchar(dat)>0]
          cat(dat,sep='\n',file=out,append=TRUE)
      }
   }
   if(software=='nm6')return()
   if(software=='nm7')invisible(do.call(rbind,lapply(run,unilog,boot=boot,project=project,...)))
 }

