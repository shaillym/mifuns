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
	infile=NULL,
	outfile=NULL,
	...
) 
{
	
  #organize arguments
  if(!is.null(udef))dosbox <- FALSE
  if(is.null(intern))intern <- if(Platform=='Windows') !dosbox else FALSE
  if(is.null(minimized))minimized <- if(Platform=='Windows') !dosbox else FALSE
  if(is.null(invisible))invisible <- if(Platform=='Windows') !dosbox else TRUE
  if(is.null(option))option <- if(nochecksum) 'nochecksum'     else  NULL
  if(is.null(perl))  perl   <- if(dosbox)     'cmd /K perl -S' else 'cmd /C perl -S'
  if(is.null(infile)) infile <- paste(filename(filename(ProjectDir,b),b,'.ctl'))
  if(is.null(outfile)) outfile <- paste(filename(filename(ProjectDir,b),b,'.lst'))
  infile <- sub('*', b, infile)
  outfile <- sub('*', b, outfile)

  #draft a command
  if(Platform == 'Mac' & grid==FALSE) nm1 <- regCommand(NMcom,ProjectDir,b,infile=infile,outfile=outfile)              
  if(Platform == 'Mac' & grid==TRUE ) nm1 <- grdCommand(NMcom,ProjectDir,b,concurrent,boot,infile=infile,outfile=outfile,...)
  if(Platform == 'Windows')nm1 <- regCommand(NMcom,ProjectDir, b, perl=perl,option=option,infile=infile,outfile=outfile,...)
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
	boot,
  	NMloc = gsub('.pl?', '', NMcom),
	nmhome = '/common/NONMEM',
	lim = '/',
	lead = 'qsub -V -j y',
	que = '-q all.q',
	SGEflgs=NULL,
	run = paste('-N Run', b, sep = ''),
	sync = '-sync y',
	shelby = '-shell n -b y',
	cwd = paste('-cwd ', nmhome, lim, NMloc, lim, 'test', lim,NMcom, sep = ''),
	infile,
	outfile,
	end = '',
	...
){
  if (concurrent & (boot == 1 | boot == 2))que <- '-q bootstrap.q'
  if (concurrent & (boot == 1 | boot == 3))sync <- ''
  if (concurrent & (boot == 1 | boot == 3))end <- '&'
  command <- paste(lead, que, SGEflgs, run, sync, shelby, cwd, infile, outfile, end)
  command
}

regCommand <- function(NMcom,ProjectDir,b,perl='perl -S',option=NULL,infile,outfile,...)paste(
	perl,
	NMcom,
	infile,
	outfile,
	option=NULL
)

