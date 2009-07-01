`runnm` <-
  function (
  	NMcom, 
	i, 
	boot, 
	concurrent, 
	Platform, 
	SGEflgs, 
	dosbox, 
	nochecksum, 
	grid, 
	udef,
	perl=NULL,
	option=NULL,
	...
) 
{
  #organize arguments
  if(!is.null(udef))dosbox <- FALSE
  intern    <- !dosbox
  minimized <- !dosbox
  invisible <- !dosbox
  if(Platform == 'Mac') invisble <- TRUE
  if(is.null(option))option <- if(nochecksum) 'nochecksum'     else  NULL
  if(is.null(perl))  perl   <- if(dosbox)     'cmd /K perl -S' else 'cmd /C perl -S'

  #draft a command
  if(Platform == 'Mac' & grid==FALSE) nm1 <- regCommand(NMcom,i)              
  if(Platform == 'Mac' & grid==TRUE ) nm1 <- grdCommand(NMcom,concurrent,boot,...)
  if(Platform == 'Windows')nm1 <- regCommand(NMcom, i, perl=perl,option=option,...)
  if(!is.null(udef))nm1 <- udef #trumps above
	  
  #run the command
  system(nm1, intern = intern, minimized = minimized, invisible = invisible)  
}

grdCommand <- function(
        NMcom,
	concurrent,
	boot
  	NMloc = gsub('.pl?', '', NMcom),
	nmhome = '/common/NONMEM',
	lim = '/',
	lead = 'qsub -V -j y',
	que = '-q all.q',
	run = paste('-N Run', i, sep = ''),
	sync = '-sync y',
	shelby = '-shell n -b y',
	cwd = paste('-cwd ', nmhome, lim, NMloc, lim, 'test', lim,NMcom, sep = ''),
	files = paste(i, '.ctl ', i, '.lst', sep = ''),
	end = '',
){	
  if (concurrent & (boot == 1 | boot == 2))que <- '-q bootstrap.q'
  if (concurrent & (boot == 1 | boot == 3))sync <- ''
  if (concurrent & (boot == 1 | boot == 3))end <- '&'
  command <- paste(lead, que, SGEflgs, run, sync, shelby, cwd, files, end)
  command
}

regCommand <- function(NMcom,i,perl='perl -S',ctlx='.ctl',lstx='.lst',option=NULL,...)paste(
	perl,
	NMcom,
	paste(sep='',i,ctlx),
	paste(sep='',i,lstx),
	option=NULL
)

