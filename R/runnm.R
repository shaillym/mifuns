`runCommand` <-
  function (
  	NMcom, 
	ProjectDir,
	b, 
	boot, 
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
	ctlfile=NULL,
	outfile=NULL,
	...
){
	
  #organize arguments
  if(!is.null(udef))dosbox <- FALSE
  if(is.null(intern))intern <- if(win()) !dosbox else FALSE
  if(is.null(minimized))minimized <- if(win()) !dosbox else FALSE
  if(is.null(invisible))invisible <- if(win()) !dosbox else TRUE
  if(is.null(option))option <- if(nochecksum) 'nochecksum'     else  NULL
  if(is.null(perl))  perl   <- if(dosbox)     'cmd /K perl -S' else 'cmd /C perl -S'
  if(is.null(ctlfile)) ctlfile <- filename(filename(ProjectDir,b),b,'.ctl')
  if(is.null(outfile)) outfile <- filename(filename(ProjectDir,b),b,'.lst')
  ctlfile <- star(ctlfile,b)
  outfile <- star(outfile,b)

  #draft a command
  if(nix() & grid==FALSE) command <- regCommand(NMcom,ProjectDir,b,ctlfile=ctlfile,outfile=outfile)              
  if(nix() & grid==TRUE ) command <- grdCommand(NMcom,ProjectDir,b,boot,ctlfile=ctlfile,outfile=outfile,...)
  if(win()) command <- regCommand(NMcom,ProjectDir, b, perl=perl,option=option,ctlfile=ctlfile,outfile=outfile,...)
  if(!is.null(udef))command <- udef #trumps above
	  
  #run the command
  suppressWarnings(system(command, intern = intern, minimized = minimized, invisible = invisible))
  
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
	boot,
  	nmhome = '/common/NONMEM',
	V='',
	j='y',
	q='all.q',
	SGEflgs=NULL,
	N=paste('Run',b,sep=''),
	sync = 'n',
	shell='n',
	B='y',
	ctlfile,
	outfile,
	...
){
  if(!file.exists(NMcom)){
      #grab the last path element.
      exec <- rev(strsplit(NMcom,'/')[[1]])[[1]]
      #guess the nonmemdir
      nmdr <- gsub('.pl?','',exec)
      #guess the full path and command
      full <- paste(nmhome,nmdr,'test',NMcom,sep='/')
      if(!file.exists(full))stop(paste('not found:\n',NMcom,'\n',full,'\n'))
      NMcom <- full
  }
  if (boot %in% c(1,2))que <- 'bootstrap.q'
  if (boot %in% c(0,2))sync <- 'y'
  command <- paste(
  	'qsub -V',
	'-j',j,
	'-q',q,
	SGEflgs,
	'-N',N,
	'-sync',sync,
	'-shell',shell,
	'-b',B,
	'-cwd',NMcom,
	ctlfile,
	outfile
  )
  if(boot %in% c(1,3)) command <- paste(command,'&')
  command
}

regCommand <- function(NMcom,ProjectDir,b,perl='perl -S',option=NULL,ctlfile,outfile,...)paste(
	perl,
	NMcom,
	ctlfile,
	outfile,
	option=NULL
)

