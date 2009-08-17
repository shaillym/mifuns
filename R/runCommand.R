`runCommand` <-
  function (
  	NMcom,
	rdir,
	b,
	boot,
	urgent,
	SGEflgs,
	invisible,
	nochecksum,
	grid,
	udef,
	ctlfile,
	outfile,
	perl=NULL,
	option=NULL,
	intern=invisible,
	minimized=invisible,
	invisible=FALSE,
	...
){
	
  #organize arguments
  if(nix()){#defaults
	  internal <- FALSE
	  minimized <- FALSE
	  invisible <- TRUE
  }
  if(is.null(option))option <- if(nochecksum) 'nochecksum' else  NULL
  if(is.null(perl)) if(nix()) perl <- 'perl -S'
  if(is.null(perl)) if(win()) perl <- if(!invisible) 'cmd /K perl -S' else 'cmd /C perl -S'

  #draft a command
  command <- regCommand(NMcom,ctlfile,outfile,perl,option,...)
  if(grid) command <- grdCommand(NMcom,ctlfile,outfile,boot,urgent,N=paste('Run',b,sep=''),o=rdir,e=rdir,...)
  if(!is.null(udef))command <- udef #trumps above

  #set up the call
  system(command, intern=intern, minimized=minimized, invisible=invisible))
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
	q=NULL,
	SGEflgs=NULL,
	sync = 'y',
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
  que <- 'all.q'
  if (!urgent) que <- 'bootstrap.q'
  if(is.null(q)) q <- que	  
  if (boot) sync <- 'n'
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

