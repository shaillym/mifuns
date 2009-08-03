`runnm` <-
  function (
  	NMcom, 
	ProjectDir,
	b, 
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
	intern=NULL,
	minimized=NULL,
	invisible=NULL,
	...
) 
{
	
  #organize arguments
  if(!is.null(udef))dosbox <- FALSE
  if(is.null(intern))intern <- if(Platform=='Windows') !dosbox else FALSE
  if(is.null(minimized)minimized <- if(Platform=='Windows') !dosbox else FALSE
  if(is.null(invisible)invisible <- if(Platform=='Windows') !dosbox else TRUE
  if(is.null(option))option <- if(nochecksum) 'nochecksum'     else  NULL
  if(is.null(perl))  perl   <- if(dosbox)     'cmd /K perl -S' else 'cmd /C perl -S'

  #draft a command
  if(Platform == 'Mac' & grid==FALSE) nm1 <- regCommand(NMcom,ProjectDir,b)              
  if(Platform == 'Mac' & grid==TRUE ) nm1 <- grdCommand(NMcom,ProjctDir,b,concurrent,boot,...)
  if(Platform == 'Windows')nm1 <- regCommand(NMcom,ProjectDir, b, perl=perl,option=option,...)
  if(!is.null(udef))nm1 <- udef #trumps above
	  
  #run the command
  suppressWarnings(system(nm1, intern = intern, minimized = minimized, invisible = invisible))
  
#  Tim,
#'minimized' and 'invisible' have no effect on the mac but do on windows. On a Mac, values for minimized and invisible are accepted but yield a warning.  intern=FALSE is the default and I believe we simply set intern=F on the Windows platform.  I just set this variable (intern) to be explicit about it.
#Bill
#- Hide quoted text -
#On Jul 2, 2009, at 11:17 AM, Tim Bergsma wrote:
#    Bill, are the defaults the same on Windows as on Mac for the system()
#    args 'intern', 'minimized', and 'invisible'?

}

grdCommand <- function(
        NMcom,
	ProjectDir,
	b,
	concurrent,
	boot
  	NMloc = gsub('.pl?', '', NMcom),
	nmhome = '/common/NONMEM',
	lim = '/',
	lead = 'qsub -V -j y',
	que = '-q all.q',
	run = paste('-N Run', b, sep = ''),
	sync = '-sync y',
	shelby = '-shell n -b y',
	cwd = paste('-cwd ', nmhome, lim, NMloc, lim, 'test', lim,NMcom, sep = ''),
	files = paste(filename(ProjectDir,b,'.ctl'),filename(ProjectDir, b,'.lst')),
	end = '',
){	
  if (concurrent & (boot == 1 | boot == 2))que <- '-q bootstrap.q'
  if (concurrent & (boot == 1 | boot == 3))sync <- ''
  if (concurrent & (boot == 1 | boot == 3))end <- '&'
  command <- paste(lead, que, SGEflgs, run, sync, shelby, cwd, files, end)
  command
}

regCommand <- function(NMcom,ProjectDir,b,perl='perl -S',option=NULL,...)paste(
	perl,
	NMcom,
	filename(ProjectDir,b,'.ctl'),
	filename(ProjectDir,b,'.lst'),
	option=NULL
)

