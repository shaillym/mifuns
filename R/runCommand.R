`runCommand` <-
  function (
  	NMcom,
	rdir,
	b,
	boot,
	urgent,
	SGEflgs,
	dosbox,
	nochecksum,
	grid,
	udef,
	ctlfile,
	outfile,
	perl=NULL,
	option=NULL,
	intern=NULL,
	minimized=NULL,
	invisible=NULL,
	...
){
	
  #organize arguments
  if(!is.null(udef))dosbox <- FALSE
  if(is.null(intern))intern <- if(win()) !dosbox else FALSE
  if(is.null(minimized))minimized <- if(win()) !dosbox else FALSE
  if(is.null(invisible))invisible <- if(win()) !dosbox else TRUE
  if(is.null(option))option <- if(nochecksum) 'nochecksum' else  NULL
  if(is.null(perl)) if(nix()) perl <- 'perl -S'
  if(is.null(perl)) if(win()) perl <- if(dosbox) 'cmd /K perl -S' else 'cmd /C perl -S'

  #draft a command
  command <- regCommand(NMcom,ctlfile,outfile,perl,option,...)
  if(grid) command <- grdCommand(NMcom,ctlfile,outfile,boot,urgent,N=paste('Run',b,sep=''),o=rdir,e=rdir,...)
  if(!is.null(udef))command <- udef #trumps above

  #set up the call
  run <- function(command, intern, minimized, invisible)suppressWarnings(system(command, intern=intern, minimized=minimized, invisible=invisible))
  runcall <- call('run', command, intern, minimized, invisible)
  #run the command
#  if(grid){
#      pid <- fork(function()eval(runcall)
#      wait(pid)
#  }
#  else eval(runcall)
eval(runcall)

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
	ctlfile,
	outfile,
	boot,
	urgent,
  	N,
	o,
	e,
	nmhome = '/common/NONMEM',
	V='',
	j='y',
	q='all.q',
	SGEflgs=NULL,
	sync = 'n',
	shell='n',
	b='y',
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
  if (!urgent)q <- 'bootstrap.q'
  if (!boot))sync <- 'y'
  command <- paste(
  	'qsub -V',
	'-j',j,
	'-e',e,
	'-o',o,
	'-q',q,
	SGEflgs,
	'-N',N,
	'-sync',sync,
	'-shell',shell,
	'-b',b,
	'-cwd',NMcom,
	ctlfile,
	outfile
  )
  if(boot) command <- paste(command,'&')
  command
}

regCommand <- function(
	NMcom,
	ctlfile,
	outfile,
	perl,
	option,
	...
)paste(
	perl,
	NMcom,
	ctlfile,
	outfile,
	option
)

